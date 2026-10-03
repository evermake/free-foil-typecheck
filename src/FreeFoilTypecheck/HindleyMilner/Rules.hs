{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Typing rules of the HM language for the generic engine
-- ('FreeFoilTypecheck.GeneralTypecheck').
module FreeFoilTypecheck.HindleyMilner.Rules where

import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Free.Foil as FreeFoil
import Data.Bifoldable (bifoldMap)
import Data.Bifunctor (bimap)
import Data.Bifunctor.Sum (Sum (..))
import Data.List (elemIndex, nub)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import FreeFoilTypecheck.GeneralTypecheck
import qualified FreeFoilTypecheck.HindleyMilner.Parser.Abs as Raw
import FreeFoilTypecheck.HindleyMilner.Syntax
import FreeFoilTypecheck.ScopeCheck (checkClosed)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import FreeFoilTypecheck.GeneralTypecheck
-- >>> import FreeFoilTypecheck.HindleyMilner.Syntax (Exp')

instance HMTypingSig FoilTypePattern TypeSig ExpSig where
  inferSigHM = \case
    ETrueSig -> return tBool
    EFalseSig -> return tBool
    ENatSig _ -> return tNat
    EAddSig l r -> do
      checkHM l tNat
      checkHM r tNat
      return tNat
    ESubSig l r -> do
      checkHM l tNat
      checkHM r tNat
      return tNat
    EIsZeroSig arg -> do
      checkHM arg tNat
      return tBool
    EIfSig cond then_ else_ -> do
      checkHM cond tBool
      thenType <- then_
      checkHM else_ thenType
      return thenType
    EAppSig fun arg -> do
      funType <- fun
      argType <- arg
      retType <- freshHM
      unifyHM funType (TArrow' argType retType)
      return retType
    ETypedSig term annotation -> do
      annotationType <- typeOfAnnotation annotation
      checkHM term annotationType
      return annotationType
    EForSig from to body -> do
      checkHM from tNat
      checkHM to tNat
      body [MonoType tNat]
    EAbsSig body -> do
      paramType <- freshHM
      bodyType <- body [MonoType paramType]
      return (TArrow' paramType bodyType)
    ELetSig bound body -> do
      boundType <- generalizeHM bound
      body [boundType]
    where
      tNat = injectUType TNat
      tBool = injectUType TBool

-- | The type of an annotation. Its unification variables (such as @?a@)
-- become fresh unification variables of the engine.
typeOfAnnotation :: Raw.Type -> TypeCheck (UType FoilTypePattern TypeSig) n (UType FoilTypePattern TypeSig Foil.VoidS)
typeOfAnnotation raw =
  case checkClosed convertToTypeSig (\(Raw.TPatternVar x) -> [x]) getTypeFromScopedType raw of
    Left (Raw.Ident x) -> failTypeCheck ("unbound type variable in an annotation: " ++ x)
    Right ()
      | hasForAll type_ -> failTypeCheck "polymorphic type annotations are not supported"
      | otherwise -> do
          metaVars <- mapM (const freshMetaVar) idents
          let metaVarOf = (Map.fromList (zip idents metaVars) Map.!)
          return (fromTypeWith metaVarOf type_)
  where
    type_ = toTypeClosed raw
    idents = uvarIdentsOf type_
    hasForAll :: Type n -> Bool
    hasForAll = \case
      TForAll _ _ -> True
      FreeFoil.Var _ -> False
      FreeFoil.Node node -> or (bifoldMap (\(FreeFoil.ScopedAST _ body) -> [hasForAll body]) (\t -> [hasForAll t]) node)

-- | Convert a type of the HM language to a type of the engine,
-- mapping its unification variables with the given function.
fromTypeWith :: (Raw.UVarIdent -> MetaVar) -> Type n -> UType FoilTypePattern TypeSig n
fromTypeWith metaVarOf = transAST $ \case
  TUVarSig x -> R2 (MetaVarSig (metaVarOf x))
  node -> L2 node

-- | Convert a closed type of the HM language, numbering its unification
-- variables in the order of their first occurrence.
fromTypeClosed :: Type' -> UType FoilTypePattern TypeSig Foil.VoidS
fromTypeClosed type_ = fromTypeWith metaVarOf type_
  where
    idents = uvarIdentsOf type_
    metaVarOf x = MetaVar (fromMaybe 0 (elemIndex x idents))

-- | Unification variables of a type of the HM language, in the order of
-- their first occurrence.
uvarIdentsOf :: Type n -> [Raw.UVarIdent]
uvarIdentsOf = nub . go
  where
    go :: Type n -> [Raw.UVarIdent]
    go = \case
      TUVar x -> [x]
      FreeFoil.Var _ -> []
      FreeFoil.Node node -> bifoldMap (\(FreeFoil.ScopedAST _ body) -> go body) go node

-- | Convert a type of the engine back to the HM language.
fromUType :: UType FoilTypePattern TypeSig n -> Type n
fromUType = \case
  FreeFoil.Var x -> FreeFoil.Var x
  FreeFoil.Node (L2 node) -> FreeFoil.Node (bimap fromUScopedType fromUType node)
  FreeFoil.Node (R2 (MetaVarSig (MetaVar i))) -> FreeFoil.Node (TUVarSig (Raw.UVarIdent ("?u" ++ show i)))
  where
    fromUScopedType (FreeFoil.ScopedAST binder body) = FreeFoil.ScopedAST binder (fromUType body)

instance Show (UType FoilTypePattern TypeSig n) where
  show = show . fromUType

-- | Show a type scheme with @forall@s.
--
-- >>> either id showHMType (inferTypeSchemeClosed LevelBased ("let id = λx. x in id" :: Exp'))
-- "forall x0 . x0 -> x0"
showHMType :: HMType (UType FoilTypePattern TypeSig) -> String
showHMType = \case
  MonoType type_ -> show type_
  PolyType (TypeScheme binders type_) -> show (foralls binders (fromUType type_))
  where
    foralls :: Foil.NameBinderList n l -> Type l -> Type n
    foralls Foil.NameBinderListEmpty body = body
    foralls (Foil.NameBinderListCons binder rest) body = TForAll (FoilTPatternVar binder) (foralls rest body)

-- | Infer the type of a closed term with the generic engine.
--
-- >>> testInferTypeNewClosed "1 + 2"
-- Right Nat
-- >>> testInferTypeNewClosed "1 + true"
-- Left "cannot unify"
testInferTypeNewClosed :: Exp Foil.VoidS -> Either String (UType FoilTypePattern TypeSig Foil.VoidS)
testInferTypeNewClosed e = fst <$> runTypeCheck (inferTypeNewClosed e) emptyTypingContext
