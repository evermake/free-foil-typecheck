{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A generic engine for Hindley–Milner type inference over free-foil syntax.
--
-- The engine knows nothing about a particular language. A language provides
-- the signature of its terms and of its types, and one typing rule per node
-- (an instance of 'HMTypingSig'). The engine provides unification variables,
-- unification, generalisation, instantiation, and the typing environment.
module FreeFoilTypecheck.GeneralTypecheck where

import Control.Monad (ap)
import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Foil.Internal as Foil
import qualified Control.Monad.Foil.Relative as Foil
import qualified Control.Monad.Free.Foil as FreeFoil
import Data.Bifoldable
import Data.Bifunctor
import Data.Bifunctor.Sum
import Data.Bifunctor.TH
import Data.Bitraversable (Bitraversable (..))
import qualified Data.Foldable as F
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Kind as K
import Data.List (nub)

-- * Unification variables

-- | A unification variable (metavariable).
newtype MetaVar = MetaVar Int
  deriving (Eq, Ord, Show)

-- | A signature with a single kind of node, a unification variable.
-- The engine sums it with the type signature of a language (see 'UType').
newtype MetaVarSig scope term = MetaVarSig MetaVar
  deriving (Eq, Show, Functor)

deriveBifunctor ''MetaVarSig
deriveBifoldable ''MetaVarSig
deriveBitraversable ''MetaVarSig

instance FreeFoil.ZipMatch MetaVarSig where
  zipMatch (MetaVarSig x) (MetaVarSig y)
    | x == y = Just (MetaVarSig x)
    | otherwise = Nothing

-- | Types of a language extended with unification variables.
type UType binder typeSig = FreeFoil.AST binder (Sum typeSig MetaVarSig)

type UScopedType binder typeSig = FreeFoil.ScopedAST binder (Sum typeSig MetaVarSig)

fromMetaVar :: MetaVar -> UType binder typeSig n
fromMetaVar x = FreeFoil.Node (R2 (MetaVarSig x))

toMetaVar :: UType binder typeSig n -> Maybe MetaVar
toMetaVar (FreeFoil.Node (R2 (MetaVarSig x))) = Just x
toMetaVar _ = Nothing

-- | Unification variables of a type, in the order of their first occurrence.
-- This does not look into the substitution (see 'freeMetaVars').
metaVarsOf :: (Bifoldable typeSig) => UType binder typeSig n -> [MetaVar]
metaVarsOf = nub . go
  where
    go :: (Bifoldable typeSig) => UType binder typeSig n -> [MetaVar]
    go (toMetaVar -> Just x) = [x]
    go (FreeFoil.Var _) = []
    go (FreeFoil.Node node) = bifoldMap (\(FreeFoil.ScopedAST _ body) -> go body) go node

-- * Type schemes

-- | Type scheme (polytype) @∀ x₁ x₂ … xₙ. T@.
-- The quantified variables are foil binders.
data TypeScheme ty where
  TypeScheme :: Foil.NameBinderList Foil.VoidS n -> ty n -> TypeScheme ty

data HMType ty
  = MonoType (ty Foil.VoidS)
  | PolyType (TypeScheme ty)

toTypeScheme :: HMType ty -> TypeScheme ty
toTypeScheme (PolyType typeScheme) = typeScheme
toTypeScheme (MonoType ty) = TypeScheme Foil.NameBinderListEmpty ty

-- | Quantify the given unification variables of a type.
generalize :: (Bifunctor typeSig, Foil.CoSinkable binder) => [MetaVar] -> UType binder typeSig Foil.VoidS -> HMType (UType binder typeSig)
generalize [] type_ = MonoType type_
generalize xs type_ = withGeneralizedVars Foil.emptyScope [] xs $ \freshNameBinders env ->
  case (Foil.assertExt freshNameBinders, Foil.assertDistinct freshNameBinders) of
    (Foil.Ext, Foil.Distinct) ->
      let env' = IntMap.fromList [(i, FreeFoil.Var name) | (MetaVar i, name) <- env]
       in PolyType (TypeScheme freshNameBinders (zonkWith (`IntMap.lookup` env') (Foil.sink type_)))

withGeneralizedVars ::
  (Foil.Distinct n) =>
  Foil.Scope n ->
  [(a, Foil.Name n)] ->
  [a] ->
  (forall l. Foil.NameBinderList n l -> [(a, Foil.Name l)] -> r) ->
  r
withGeneralizedVars scope env xs cont =
  case xs of
    [] -> cont Foil.NameBinderListEmpty (map (fmap Foil.sink) env)
    y : ys -> Foil.withFresh scope $ \binder ->
      let scope' = Foil.extendScope binder scope
          env' = (y, Foil.nameOf binder) : map (fmap Foil.sink) env
       in withGeneralizedVars scope' env' ys $ \nameBinderList env'' ->
            cont (Foil.NameBinderListCons binder nameBinderList) env''

-- | Instantiate the quantified variables of a type scheme with the given types.
instantiateWith :: (Bifunctor typeSig, Foil.CoSinkable binder) => [UType binder typeSig Foil.VoidS] -> TypeScheme (UType binder typeSig) -> UType binder typeSig Foil.VoidS
instantiateWith types (TypeScheme binders body) =
  FreeFoil.substitutePattern Foil.emptyScope Foil.identitySubst binders types body

-- | Number of variables bound by a pattern.
patternSize :: (Foil.CoSinkable pattern) => pattern n l -> Int
patternSize = go . Foil.nameBinderListOf
  where
    go :: Foil.NameBinderList n l -> Int
    go Foil.NameBinderListEmpty = 0
    go (Foil.NameBinderListCons _ rest) = 1 + go rest

-- | A canonical form of a type scheme: all unification variables are
-- quantified, in the order of their first occurrence.
-- Two types are equal up to renaming of variables if and only if
-- their canonical forms are α-equivalent.
canonicalHMType :: (Bifunctor typeSig, Bifoldable typeSig, Foil.CoSinkable binder) => HMType (UType binder typeSig) -> HMType (UType binder typeSig)
canonicalHMType hmType =
  case toTypeScheme hmType of
    scheme@(TypeScheme binders body) ->
      let -- fresh unification variables that do not occur in the type
          offset = 1 + maximum (0 : [i | MetaVar i <- metaVarsOf body])
          type_ = instantiateWith [fromMetaVar (MetaVar (offset + i)) | i <- [0 .. patternSize binders - 1]] scheme
       in generalize (metaVarsOf type_) type_

-- * Substitution

-- | Apply a (triangular) substitution of unification variables.
-- The lookup function is given in the current scope, so it is sunk under binders.
zonkWith ::
  (Bifunctor typeSig, Foil.CoSinkable binder, Foil.Distinct n) =>
  (Int -> Maybe (UType binder typeSig n)) ->
  UType binder typeSig n ->
  UType binder typeSig n
zonkWith look = \case
  type_@(toMetaVar -> Just (MetaVar i)) -> maybe type_ (zonkWith look) (look i)
  FreeFoil.Var x -> FreeFoil.Var x
  FreeFoil.Node node -> FreeFoil.Node (bimap (zonkScopedWith look) (zonkWith look) node)

zonkScopedWith ::
  (Bifunctor typeSig, Foil.CoSinkable binder, Foil.Distinct n) =>
  (Int -> Maybe (UType binder typeSig n)) ->
  UScopedType binder typeSig n ->
  UScopedType binder typeSig n
zonkScopedWith look (FreeFoil.ScopedAST binder body) =
  case (Foil.assertExt binder, Foil.assertDistinct binder) of
    (Foil.Ext, Foil.Distinct) ->
      FreeFoil.ScopedAST binder (zonkWith (fmap Foil.sink . look) body)

-- | Apply a substitution of unification variables to a closed type.
zonk :: (Bifunctor typeSig, Foil.CoSinkable binder) => IntMap.IntMap (UType binder typeSig Foil.VoidS) -> UType binder typeSig Foil.VoidS -> UType binder typeSig Foil.VoidS
zonk subst = zonkWith (`IntMap.lookup` subst)

-- | Unification variables of a type after applying a substitution
-- (possibly with repetitions).
freeMetaVars :: (Bifoldable typeSig) => IntMap.IntMap (UType binder typeSig Foil.VoidS) -> UType binder typeSig n -> [Int]
freeMetaVars subst = \case
  (toMetaVar -> Just (MetaVar i)) -> case IntMap.lookup i subst of
    Just type_ -> freeMetaVars subst type_
    Nothing -> [i]
  FreeFoil.Var _ -> []
  FreeFoil.Node node -> bifoldMap (\(FreeFoil.ScopedAST _ body) -> freeMetaVars subst body) (freeMetaVars subst) node

freeMetaVarsHM :: (Bifoldable typeSig) => IntMap.IntMap (UType binder typeSig Foil.VoidS) -> HMType (UType binder typeSig) -> [Int]
freeMetaVarsHM subst (MonoType type_) = freeMetaVars subst type_
freeMetaVarsHM subst (PolyType (TypeScheme _ type_)) = freeMetaVars subst type_

-- * Inference monad

data TypingContext ty n = TypingContext
  { -- | Triangular substitution of unification variables.
    tcSubst :: IntMap.IntMap (ty Foil.VoidS),
    -- | Types of the variables in scope.
    tcTypings :: Foil.NameMap n (HMType ty),
    -- | Next unification variable.
    tcFreshId :: Int
  }

emptyTypingContext :: TypingContext ty Foil.VoidS
emptyTypingContext = TypingContext IntMap.empty Foil.emptyNameMap 0

newtype TypeCheck ty n a = TypeCheck {runTypeCheck :: TypingContext ty n -> Either String (a, TypingContext ty n)}
  deriving (Functor)

instance Applicative (TypeCheck ty n) where
  pure x = TypeCheck $ \tc -> Right (x, tc)
  (<*>) = ap

instance Monad (TypeCheck ty n) where
  TypeCheck g >>= f = TypeCheck $ \tc -> do
    (x, tc') <- g tc
    runTypeCheck (f x) tc'

get :: TypeCheck ty n (TypingContext ty n)
get = TypeCheck $ \tc -> Right (tc, tc)

put :: TypingContext ty n -> TypeCheck ty n ()
put new = TypeCheck $ \_old -> Right ((), new)

failTypeCheck :: String -> TypeCheck ty n a
failTypeCheck msg = TypeCheck (\_ctx -> Left msg)

-- | Run a computation in a scope extended with a pattern.
-- The list gives the types of the variables bound by the pattern, in order.
enterScope ::
  (Foil.CoSinkable binder) =>
  binder n l ->
  [HMType ty] ->
  TypeCheck ty l a ->
  TypeCheck ty n a
enterScope binder types (TypeCheck action)
  | patternSize binder /= length types =
      failTypeCheck
        ("pattern binds " ++ show (patternSize binder) ++ " variable(s), but the typing rule gives " ++ show (length types) ++ " type(s)")
  | otherwise = TypeCheck $ \ctx -> do
      let typings = tcTypings ctx
      (x, ctx') <- action ctx {tcTypings = Foil.addNameBinders binder types typings}
      return (x, ctx' {tcTypings = typings})

-- * Interface for typing rules

type Infer typeBinder typeSig = UType typeBinder typeSig Foil.VoidS

type ScopedInfer typeBinder typeSig n =
  Maybe (HMType (UType typeBinder typeSig)) ->
  TypeCheck (UType typeBinder typeSig) n (Infer typeBinder typeSig, Infer typeBinder typeSig)

class HMTypingSig (binder :: Foil.S -> Foil.S -> K.Type) (typeSig :: K.Type -> K.Type -> K.Type) (sig :: K.Type -> K.Type -> K.Type) where
  inferSigHM ::
    sig
      (ScopedInfer binder typeSig n)
      (Infer binder typeSig) -> -- expr node
    TypeCheck (UType binder typeSig) n (Infer binder typeSig) -- typecheck result

-- * Operations for typing rules

-- | A fresh unification variable.
freshMetaVar :: TypeCheck ty n MetaVar
freshMetaVar = do
  ctx <- get
  put ctx {tcFreshId = tcFreshId ctx + 1}
  return (MetaVar (tcFreshId ctx))

-- | A fresh unification variable as a type.
freshHM :: TypeCheck (UType binder typeSig) n (UType binder typeSig Foil.VoidS)
freshHM = fromMetaVar <$> freshMetaVar

-- | Unify two types, extending the substitution.
unifyHM :: (FreeFoil.ZipMatch typeSig, Bitraversable typeSig, Foil.CoSinkable binder) => UType binder typeSig Foil.VoidS -> UType binder typeSig Foil.VoidS -> TypeCheck (UType binder typeSig) n ()
unifyHM typ1 typ2 = do
  subst <- tcSubst <$> get
  case (walk subst typ1, walk subst typ2) of
    (toMetaVar -> Just x, toMetaVar -> Just y)
      | x == y -> return ()
    (toMetaVar -> Just x, r) -> bindMetaVar x r
    (l, toMetaVar -> Just x) -> bindMetaVar x l
    (FreeFoil.Node l, FreeFoil.Node r) ->
      case FreeFoil.zipMatch l r of
        Nothing -> failTypeCheck "cannot unify"
        Just lr -> bitraverse_ (\_ -> failTypeCheck "cannot unify types with binders") (uncurry unifyHM) lr
    (_, _) -> failTypeCheck "cannot unify"
  where
    -- resolve a bound unification variable at the root
    walk subst type_@(toMetaVar -> Just (MetaVar i)) = maybe type_ (walk subst) (IntMap.lookup i subst)
    walk _ type_ = type_

-- | Bind a unification variable (after the occurs check).
bindMetaVar :: (Bifoldable typeSig) => MetaVar -> UType binder typeSig Foil.VoidS -> TypeCheck (UType binder typeSig) n ()
bindMetaVar (MetaVar x) type_ = do
  ctx <- get
  if x `elem` freeMetaVars (tcSubst ctx) type_
    then failTypeCheck "occurs check failed"
    else put ctx {tcSubst = IntMap.insert x type_ (tcSubst ctx)}

-- | Generalise a type over the unification variables that do not occur
-- in the typing environment.
generalizeHM ::
  (Bifunctor typeSig, Bifoldable typeSig, Foil.CoSinkable binder) =>
  UType binder typeSig Foil.VoidS ->
  TypeCheck (UType binder typeSig) n (HMType (UType binder typeSig))
generalizeHM type_ = do
  ctx <- get
  let subst = tcSubst ctx
      type' = zonk subst type_
      envVars = IntSet.fromList (concatMap (freeMetaVarsHM subst) (F.toList (tcTypings ctx)))
      vars = [x | x@(MetaVar i) <- metaVarsOf type', not (IntSet.member i envVars)]
  return (generalize vars type')

-- | Instantiate a type scheme with fresh unification variables.
instantiateHM :: (Bifunctor typeSig, Foil.CoSinkable binder) => HMType (UType binder typeSig) -> TypeCheck (UType binder typeSig) n (UType binder typeSig Foil.VoidS)
instantiateHM (MonoType type_) = return type_
instantiateHM (PolyType scheme@(TypeScheme binders _)) = do
  types <- mapM (const freshHM) [1 .. patternSize binders]
  return (instantiateWith types scheme)

-- | Apply the current substitution to a type.
zonkHM :: (Bifunctor typeSig, Foil.CoSinkable binder) => UType binder typeSig Foil.VoidS -> TypeCheck (UType binder typeSig) n (UType binder typeSig Foil.VoidS)
zonkHM type_ = do
  ctx <- get
  return (zonk (tcSubst ctx) type_)

-- * Generic traversal

inferTypeNewClosed ::
  (Foil.CoSinkable typeBinder, Bitraversable sig, Foil.CoSinkable binder, HMTypingSig typeBinder typeSig sig, Bifunctor typeSig) =>
  FreeFoil.AST binder sig Foil.VoidS ->
  TypeCheck (UType typeBinder typeSig) Foil.VoidS (UType typeBinder typeSig Foil.VoidS)
inferTypeNewClosed expr = reconstructType expr >>= zonkHM

reconstructType ::
  (Foil.CoSinkable typeBinder, Bitraversable sig, Foil.CoSinkable binder, Bifunctor typeSig, HMTypingSig typeBinder typeSig sig) =>
  FreeFoil.AST binder sig n ->
  TypeCheck (UType typeBinder typeSig) n (UType typeBinder typeSig Foil.VoidS)
reconstructType = \case
  FreeFoil.Var x -> do
    ctx <- get
    instantiateHM (Foil.lookupName x (tcTypings ctx))
  FreeFoil.Node node -> do
    node' <- bitraverse reconstructTypeScoped' reconstructType node
    inferSigHM node'

reconstructTypeScoped' ::
  (Foil.CoSinkable typeBinder, Bitraversable sig, Foil.CoSinkable binder, Bifunctor typeSig, HMTypingSig typeBinder typeSig sig) =>
  FreeFoil.ScopedAST binder sig n ->
  TypeCheck (UType typeBinder typeSig) n (ScopedInfer typeBinder typeSig n)
reconstructTypeScoped' (FreeFoil.ScopedAST binder body) =
  return $ \case
    Nothing -> do
      type_ <- freshHM
      bodyType <- enterScope binder [MonoType type_] (reconstructType body)
      return (type_, bodyType)
    Just gtype -> do
      bodyType <- enterScope binder [gtype] (reconstructType body)
      type_ <- instantiateHM gtype
      return (type_, bodyType)

-- * Comparing types

class AlphaEquiv t where
  alphaEquiv :: (Foil.Distinct n) => Foil.Scope n -> t n -> t n -> Bool

instance
  (Bifunctor sig, Bifoldable sig, FreeFoil.ZipMatch sig, Foil.UnifiablePattern binder) =>
  AlphaEquiv (FreeFoil.AST binder sig)
  where
  alphaEquiv = FreeFoil.alphaEquiv

equivHMType :: (Foil.RelMonad Foil.Name ty) => (forall n. (Foil.Distinct n) => Foil.Scope n -> ty n -> ty n -> Bool) -> HMType ty -> HMType ty -> Bool
equivHMType alphaEquivFunc t1 t2 =
  equivTypeScheme alphaEquivFunc (toTypeScheme t1) (toTypeScheme t2)

equivTypeScheme :: (Foil.RelMonad Foil.Name ty) => (forall n. (Foil.Distinct n) => Foil.Scope n -> ty n -> ty n -> Bool) -> TypeScheme ty -> TypeScheme ty -> Bool
equivTypeScheme alphaEquivFunc (TypeScheme binders1 ty1) (TypeScheme binders2 ty2) =
  case Foil.unifyPatterns binders1 binders2 of
    Foil.SameNameBinders {} ->
      case Foil.assertDistinct binders1 of
        Foil.Distinct ->
          let scope = Foil.extendScopePattern binders1 Foil.emptyScope
           in alphaEquivFunc scope ty1 ty2
    Foil.RenameLeftNameBinder _ rename1to2 ->
      case Foil.assertDistinct binders2 of
        Foil.Distinct ->
          let scope = Foil.extendScopePattern binders2 Foil.emptyScope
           in alphaEquivFunc scope (Foil.liftRM scope (Foil.fromNameBinderRenaming rename1to2) ty1) ty2
    Foil.RenameRightNameBinder _ rename2to1 ->
      case Foil.assertDistinct binders1 of
        Foil.Distinct ->
          let scope = Foil.extendScopePattern binders1 Foil.emptyScope
           in alphaEquivFunc scope ty1 (Foil.liftRM scope (Foil.fromNameBinderRenaming rename2to1) ty2)
    Foil.RenameBothBinders binders' rename1 rename2 ->
      case Foil.assertDistinct binders' of
        Foil.Distinct ->
          let scope = Foil.extendScopePattern binders' Foil.emptyScope
           in alphaEquivFunc
                scope
                (Foil.liftRM scope (Foil.fromNameBinderRenaming rename1) ty1)
                (Foil.liftRM scope (Foil.fromNameBinderRenaming rename2) ty2)
    Foil.NotUnifiable -> False

-- | Equality of types up to renaming of unification and bound variables.
equivUpToRenaming :: (Bifunctor typeSig, Bifoldable typeSig, FreeFoil.ZipMatch typeSig, Foil.UnifiablePattern binder) => HMType (UType binder typeSig) -> HMType (UType binder typeSig) -> Bool
equivUpToRenaming t1 t2 = equivHMType alphaEquiv (canonicalHMType t1) (canonicalHMType t2)

-- * Helpers for languages

injectUType :: (Bifunctor typeSig) => FreeFoil.AST binder typeSig n -> UType binder typeSig n
injectUType = transAST L2

transAST ::
  (Bifunctor sig1) =>
  (forall scope term. sig1 scope term -> sig2 scope term) ->
  FreeFoil.AST binder sig1 n ->
  FreeFoil.AST binder sig2 n
transAST _ (FreeFoil.Var x) = FreeFoil.Var x
transAST phi (FreeFoil.Node node) = FreeFoil.Node (phi (bimap (transScopedAST phi) (transAST phi) node))

transScopedAST ::
  (Bifunctor sig1) =>
  (forall scope term. sig1 scope term -> sig2 scope term) ->
  FreeFoil.ScopedAST binder sig1 n ->
  FreeFoil.ScopedAST binder sig2 n
transScopedAST phi (FreeFoil.ScopedAST binder body) =
  FreeFoil.ScopedAST binder (transAST phi body)

-- * Orphans

deriving instance Functor (Foil.NameMap n)

deriving instance Foldable (Foil.NameMap n)

deriving instance Traversable (Foil.NameMap n)

instance Foil.UnifiablePattern Foil.NameBinderList where
  unifyPatterns Foil.NameBinderListEmpty Foil.NameBinderListEmpty =
    Foil.SameNameBinders Foil.emptyNameBinders
  unifyPatterns Foil.NameBinderListEmpty (Foil.NameBinderListCons _ _) =
    Foil.NotUnifiable
  unifyPatterns (Foil.NameBinderListCons _ _) Foil.NameBinderListEmpty =
    Foil.NotUnifiable
  unifyPatterns (Foil.NameBinderListCons x xs) (Foil.NameBinderListCons y ys) =
    case (Foil.assertDistinct x, Foil.assertDistinct y) of
      (Foil.Distinct, Foil.Distinct) -> Foil.unifyNameBinders x y `Foil.andThenUnifyPatterns` (xs, ys)
