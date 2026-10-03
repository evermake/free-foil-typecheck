{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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

instance HMTypingSig FoilTypePattern TypeSig ExpSig where
  inferSigHM = \case
    ETrueSig -> return (injectUType TBool)
    EFalseSig -> return (injectUType TBool)
    ESubSig l r -> do
      _ <- unifyHM l (injectUType TNat)
      _ <- unifyHM r (injectUType TNat)
      return (injectUType TNat)
    EAddSig l r -> do
      _ <- unifyHM l (injectUType TNat)
      _ <- unifyHM r (injectUType TNat)
      return (injectUType TNat)
    EIfSig condType thenType elseType -> do
      _ <- unifyHM condType (injectUType TBool)
      _ <- unifyHM thenType elseType
      return thenType
    EIsZeroSig argType -> do
      _ <- unifyHM argType (injectUType TNat)
      return (injectUType TBool)
    EAppSig funType argType -> do
      retType <- freshHM
      _ <- unifyHM funType (FreeFoil.Node (L2 (TArrowSig argType retType)))
      return retType
    ETypedSig ty annotation -> do
      annotationType <- typeOfAnnotation annotation
      _ <- unifyHM ty annotationType
      return annotationType
    ENatSig _ -> do
      return (injectUType TNat)
    EForSig fromTy toTy inferBody -> do
      (binderType, bodyType) <- inferBody Nothing
      _ <- unifyHM fromTy (injectUType TNat)
      _ <- unifyHM toTy (injectUType TNat)
      _ <- unifyHM binderType (injectUType TNat)
      return bodyType
    EAbsSig inferBody -> do
      (paramType, bodyType) <- inferBody Nothing
      return (TArrow' paramType bodyType)
    ELetSig type1 inferBody -> do
      gtype1 <- generalizeHM type1
      (_, bodyType) <- inferBody (Just gtype1)
      return bodyType

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

-- | Infer the type of a closed term with the generic engine.
--
-- >>> testInferTypeNewClosed "1 + 2"
-- Right Nat
-- >>> testInferTypeNewClosed "1 + true"
-- Left "cannot unify"
testInferTypeNewClosed :: Exp Foil.VoidS -> Either String (UType FoilTypePattern TypeSig Foil.VoidS)
testInferTypeNewClosed e = fst <$> runTypeCheck (inferTypeNewClosed e) emptyTypingContext
