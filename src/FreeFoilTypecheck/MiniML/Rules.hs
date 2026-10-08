{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Typing rules of MiniML for the generic engine
-- ('FreeFoilTypecheck.GeneralTypecheck').
--
-- MiniML follows Mini-ML of Clément, Despeyroux, Despeyroux and Kahn
-- (LFP 1986) and the simple extensions in Pierce's /Types and Programming
-- Languages/, chapter 11. @grammar/miniml.cf@ gives the details.
--
-- A pattern checked against a type gives types to its variables, in order,
-- as the judgement of Mini-ML that builds the local environment of a pattern
-- (section 2.5.3 there). Mini-ML has only variables, pairs and the unit as
-- patterns; the rules for the wildcard and for the patterns of sums and lists
-- are written by analogy.
-- As in Mini-ML, @letrec p = e1 in e2@ is typed as @let p = fix p. e1 in e2@.
module FreeFoilTypecheck.MiniML.Rules where

import Control.Monad (forM_)
import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Free.Foil as FreeFoil
import Data.Bifoldable (bifoldMap)
import Data.Bifunctor (bimap)
import Data.Bifunctor.Sum (Sum (..))
import Data.List (elemIndex, nub)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import FreeFoilTypecheck.GeneralTypecheck
import FreeFoilTypecheck.MetaVar (MetaVar (..))
import FreeFoilTypecheck.MiniML.FreeFoilConfig (typeOfScopedType)
import qualified FreeFoilTypecheck.MiniML.Parser.Abs as Raw
import qualified FreeFoilTypecheck.MiniML.Parser.Par as Raw
import FreeFoilTypecheck.MiniML.Syntax
import FreeFoilTypecheck.ScopeCheck (checkClosed)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import FreeFoilTypecheck.GeneralTypecheck

-- * Typing rules

instance HMTypingSig TypePattern TypeSig ExpSig where
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
    -- functions
    EAbsSig body -> do
      paramType <- freshHM
      bodyType <- inferScopedHM body paramType
      return (tArrow paramType bodyType)
    EAppSig fun arg -> do
      funType <- fun
      argType <- arg
      resultType <- freshHM
      unifyHM funType (tArrow argType resultType)
      return resultType
    -- let-polymorphism and recursion
    ELetSig bound body -> do
      types <- generalizePatternHM (bound >>= checkBinderHM body)
      inferBodyHM body types
    ELetRecSig bound body -> do
      types <- generalizePatternHM (recursive bound >>= checkBinderHM body)
      inferBodyHM body types
    EFixSig body -> recursive body
    -- pairs
    EPairSig first second -> do
      firstType <- first
      secondType <- second
      return (tProd firstType secondType)
    EFstSig pair -> do
      firstType <- freshHM
      secondType <- freshHM
      checkHM pair (tProd firstType secondType)
      return firstType
    ESndSig pair -> do
      firstType <- freshHM
      secondType <- freshHM
      checkHM pair (tProd firstType secondType)
      return secondType
    -- sums
    EInlSig left -> do
      leftType <- left
      rightType <- freshHM
      return (tSum leftType rightType)
    EInrSig right -> do
      leftType <- freshHM
      rightType <- right
      return (tSum leftType rightType)
    -- lists
    ENilSig -> tList <$> freshHM
    EConsSig hd tl -> do
      elemType <- hd
      checkHM tl (tList elemType)
      return (tList elemType)
    -- every branch matches the scrutinee, and all branches have one type
    ECaseSig scrutinee branches -> do
      scrutineeType <- scrutinee
      resultType <- freshHM
      forM_ branches $ \(BranchSig branch) -> do
        branchType <- inferScopedHM branch scrutineeType
        unifyHM branchType resultType
      return resultType
    -- annotations
    ETypedSig term annotation -> do
      annotationType <- typeOfAnnotation annotation
      checkHM term annotationType
      return annotationType
    where
      -- the body of @fix p. e@ (or the bound term of @letrec p = e in …@),
      -- where the variables of @p@ are monomorphic and @p@ has the type of @e@
      recursive scoped = do
        selfType <- freshHM
        bodyType <- inferScopedHM scoped selfType
        unifyHM selfType bodyType
        return selfType

-- | Typing rules of patterns. A pattern checked against a type gives the
-- types of its variables, from left to right.
instance HMTypingPattern TypePattern TypeSig Pattern where
  checkPatternHM pat type_ = case pat of
    PatternWildcard -> return []
    PatternVar _ -> return [type_]
    PatternPair l r -> do
      leftType <- freshHM
      rightType <- freshHM
      unifyHM type_ (tProd leftType rightType)
      (++) <$> checkPatternHM l leftType <*> checkPatternHM r rightType
    PatternInl p -> do
      leftType <- freshHM
      rightType <- freshHM
      unifyHM type_ (tSum leftType rightType)
      checkPatternHM p leftType
    PatternInr p -> do
      leftType <- freshHM
      rightType <- freshHM
      unifyHM type_ (tSum leftType rightType)
      checkPatternHM p rightType
    PatternNil -> do
      elemType <- freshHM
      unifyHM type_ (tList elemType)
      return []
    PatternCons hd tl -> do
      elemType <- freshHM
      unifyHM type_ (tList elemType)
      (++) <$> checkPatternHM hd elemType <*> checkPatternHM tl type_

tNat, tBool :: UType TypePattern TypeSig n
tNat = injectUType TNat
tBool = injectUType TBool

tArrow, tProd, tSum :: UType TypePattern TypeSig n -> UType TypePattern TypeSig n -> UType TypePattern TypeSig n
tArrow a b = FreeFoil.Node (L2 (TArrowSig a b))
tProd a b = FreeFoil.Node (L2 (TProdSig a b))
tSum a b = FreeFoil.Node (L2 (TSumSig a b))

tList :: UType TypePattern TypeSig n -> UType TypePattern TypeSig n
tList a = FreeFoil.Node (L2 (TListSig a))

-- * Conversion of types

-- | The type of an annotation. Its unification variables (such as @?a@)
-- become fresh unification variables of the engine.
typeOfAnnotation :: Raw.Type -> TypeCheck (UType TypePattern TypeSig) n (UType TypePattern TypeSig Foil.VoidS)
typeOfAnnotation raw =
  case checkClosed toTypeSig (\(Raw.TPatternVar x) -> [x]) typeOfScopedType raw of
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

-- | Convert a type of MiniML to a type of the engine,
-- mapping its unification variables with the given function.
fromTypeWith :: (Raw.UVarIdent -> MetaVar) -> Type n -> UType TypePattern TypeSig n
fromTypeWith metaVarOf = transAST $ \case
  TUVarSig x -> R2 (MetaVarSig (metaVarOf x))
  node -> L2 node

-- | Convert a closed type of MiniML, numbering its unification variables
-- in the order of their first occurrence.
fromTypeClosed :: Type' -> UType TypePattern TypeSig Foil.VoidS
fromTypeClosed type_ = fromTypeWith metaVarOf type_
  where
    idents = uvarIdentsOf type_
    metaVarOf x = MetaVar (fromMaybe 0 (elemIndex x idents))

-- | Unification variables of a type of MiniML, in the order of their first occurrence.
uvarIdentsOf :: Type n -> [Raw.UVarIdent]
uvarIdentsOf = nub . go
  where
    go :: Type n -> [Raw.UVarIdent]
    go = \case
      TUVar x -> [x]
      FreeFoil.Var _ -> []
      FreeFoil.Node node -> bifoldMap (\(FreeFoil.ScopedAST _ body) -> go body) go node

-- | Convert a type of the engine back to MiniML.
fromUType :: UType TypePattern TypeSig n -> Type n
fromUType = \case
  FreeFoil.Var x -> FreeFoil.Var x
  FreeFoil.Node (L2 node) -> FreeFoil.Node (bimap fromUScopedType fromUType node)
  FreeFoil.Node (R2 (MetaVarSig (MetaVar i))) -> FreeFoil.Node (TUVarSig (Raw.UVarIdent ("?u" ++ show i)))
  where
    fromUScopedType (FreeFoil.ScopedAST binder body) = FreeFoil.ScopedAST binder (fromUType body)

instance Show (UType TypePattern TypeSig n) where
  show = show . fromUType

-- | Show a type scheme with @forall@s.
showHMType :: HMType (UType TypePattern TypeSig) -> String
showHMType = \case
  MonoType type_ -> show type_
  PolyType (TypeScheme binders type_) -> show (foralls binders (fromUType type_))
  where
    foralls :: Foil.NameBinderList n l -> Type l -> Type n
    foralls Foil.NameBinderListEmpty body = body
    foralls (Foil.NameBinderListCons binder rest) body = TForAll (TPatternVar binder) (foralls rest body)

-- * Running inference

-- | Parse a MiniML program and infer its type scheme.
--
-- >>> either id showHMType (inferMiniML LevelBased "letrec map = λf. λl. case l of { [] -> [] | x :: xs -> f x :: map f xs } in map")
-- "forall x0 . (forall x1 . (x0 -> x1) -> List x0 -> List x1)"
-- >>> either id showHMType (inferMiniML LevelBased "let swap = λ(x, y). (y, x) in (swap (1, true), swap (false, 2))")
-- "(Bool * Nat) * Nat * Bool"
-- >>> either id showHMType (inferMiniML LevelBased "λs. case s of { inl (x, _) -> x | inr [] -> 0 | inr (y :: _) -> y }")
-- "forall x0 . Nat * x0 + List Nat -> Nat"
-- >>> either id showHMType (inferMiniML LevelBased "λx. x x")
-- "occurs check failed"
inferMiniML :: Generalization -> String -> Either String (HMType (UType TypePattern TypeSig))
inferMiniML generalization source = do
  raw <- Raw.pExp (Raw.myLexer source)
  expr <- toExpClosedChecked raw
  inferTypeSchemeClosed generalization expr
