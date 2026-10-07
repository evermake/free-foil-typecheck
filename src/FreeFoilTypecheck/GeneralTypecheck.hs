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
-- the signature of its terms and of its types, one typing rule per node
-- (an instance of 'HMTypingSig'), and the typing rules of its patterns (an
-- instance of 'HMTypingPattern'). The engine provides unification variables,
-- unification, generalisation, instantiation, and the typing environment.
--
-- Unification variables are integers ('MetaVar'), and their bindings form a
-- triangular substitution in a 'MetaVarMap': a binding may mention other
-- variables, and bindings are never composed. 'zonkWith' resolves a type by
-- following the chains of bindings, as @zonkType@ does in the type checker of
-- Peyton Jones et al. The sources of these techniques are:
--
-- * Simon Peyton Jones, Dimitrios Vytiniotis, Stephanie Weirich and Mark
--   Shields. /Practical type inference for arbitrary-rank types/. Journal of
--   Functional Programming 17(1), 2007.
--   <https://doi.org/10.1017/S0956796806006034>. Its appendix defines
--   @zonkType@ over mutable meta type variables.
-- * William E. Byrd. /Relational programming in miniKanren: techniques,
--   applications, and implementations/. PhD thesis, Indiana University, 2009.
--   <https://scholarworks.iu.edu/dspace/items/450e1b65-70da-4a38-8e73-c182818de110>.
--   Triangular substitutions and the @walk@ lookup.
-- * Wren Romano. The Haskell library unification-fd, module
--   @Control.Unification.IntVar@: integer unification variables bound in an
--   @IntMap@. <https://hackage.haskell.org/package/unification-fd>
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

-- | The unification variable created after the given one.
nextMetaVar :: MetaVar -> MetaVar
nextMetaVar (MetaVar i) = MetaVar (i + 1)

-- | A finite map from unification variables.
newtype MetaVarMap a = MetaVarMap (IntMap.IntMap a)

emptyMetaVarMap :: MetaVarMap a
emptyMetaVarMap = MetaVarMap IntMap.empty

fromListMetaVarMap :: [(MetaVar, a)] -> MetaVarMap a
fromListMetaVarMap xs = MetaVarMap (IntMap.fromList [(i, a) | (MetaVar i, a) <- xs])

lookupMetaVarMap :: MetaVar -> MetaVarMap a -> Maybe a
lookupMetaVarMap (MetaVar i) (MetaVarMap m) = IntMap.lookup i m

findWithDefaultMetaVarMap :: a -> MetaVar -> MetaVarMap a -> a
findWithDefaultMetaVarMap def (MetaVar i) (MetaVarMap m) = IntMap.findWithDefault def i m

insertMetaVarMap :: MetaVar -> a -> MetaVarMap a -> MetaVarMap a
insertMetaVarMap (MetaVar i) a (MetaVarMap m) = MetaVarMap (IntMap.insert i a m)

adjustMetaVarMap :: (a -> a) -> MetaVar -> MetaVarMap a -> MetaVarMap a
adjustMetaVarMap f (MetaVar i) (MetaVarMap m) = MetaVarMap (IntMap.adjust f i m)

sizeMetaVarMap :: MetaVarMap a -> Int
sizeMetaVarMap (MetaVarMap m) = IntMap.size m

-- | A finite set of unification variables.
newtype MetaVarSet = MetaVarSet IntSet.IntSet

fromListMetaVarSet :: [MetaVar] -> MetaVarSet
fromListMetaVarSet xs = MetaVarSet (IntSet.fromList [i | MetaVar i <- xs])

memberMetaVarSet :: MetaVar -> MetaVarSet -> Bool
memberMetaVarSet (MetaVar i) (MetaVarSet set) = IntSet.member i set

-- | A level of generalisation: the number of enclosing 'generalizeHM's.
newtype Level = Level Int
  deriving (Eq, Ord, Show)

-- | The level outside of any 'generalizeHM'.
outermostLevel :: Level
outermostLevel = Level 0

-- | The level inside one more 'generalizeHM'.
deeperLevel :: Level -> Level
deeperLevel (Level n) = Level (n + 1)

-- | The level outside of the innermost 'generalizeHM'.
shallowerLevel :: Level -> Level
shallowerLevel (Level n) = Level (n - 1)

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
      let env' = fromListMetaVarMap [(x, FreeFoil.Var name) | (x, name) <- env]
       in PolyType (TypeScheme freshNameBinders (zonkWith (`lookupMetaVarMap` env') (Foil.sink type_)))

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

-- | Apply a triangular substitution of unification variables, following the
-- chains of bindings (zonking, see the module header).
-- The lookup function is given in the current scope, so it is sunk under binders.
zonkWith ::
  (Bifunctor typeSig, Foil.CoSinkable binder, Foil.Distinct n) =>
  (MetaVar -> Maybe (UType binder typeSig n)) ->
  UType binder typeSig n ->
  UType binder typeSig n
zonkWith look = \case
  type_@(toMetaVar -> Just x) -> maybe type_ (zonkWith look) (look x)
  FreeFoil.Var x -> FreeFoil.Var x
  FreeFoil.Node node -> FreeFoil.Node (bimap (zonkScopedWith look) (zonkWith look) node)

zonkScopedWith ::
  (Bifunctor typeSig, Foil.CoSinkable binder, Foil.Distinct n) =>
  (MetaVar -> Maybe (UType binder typeSig n)) ->
  UScopedType binder typeSig n ->
  UScopedType binder typeSig n
zonkScopedWith look (FreeFoil.ScopedAST binder body) =
  case (Foil.assertExt binder, Foil.assertDistinct binder) of
    (Foil.Ext, Foil.Distinct) ->
      FreeFoil.ScopedAST binder (zonkWith (fmap Foil.sink . look) body)

-- | Apply a substitution of unification variables to a closed type.
zonk :: (Bifunctor typeSig, Foil.CoSinkable binder) => MetaVarMap (UType binder typeSig Foil.VoidS) -> UType binder typeSig Foil.VoidS -> UType binder typeSig Foil.VoidS
zonk subst = zonkWith (`lookupMetaVarMap` subst)

-- | Unification variables of a type after applying a substitution
-- (possibly with repetitions).
freeMetaVars :: (Bifoldable typeSig) => MetaVarMap (UType binder typeSig Foil.VoidS) -> UType binder typeSig n -> [MetaVar]
freeMetaVars subst = \case
  (toMetaVar -> Just x) -> case lookupMetaVarMap x subst of
    Just type_ -> freeMetaVars subst type_
    Nothing -> [x]
  FreeFoil.Var _ -> []
  FreeFoil.Node node -> bifoldMap (\(FreeFoil.ScopedAST _ body) -> freeMetaVars subst body) (freeMetaVars subst) node

freeMetaVarsHM :: (Bifoldable typeSig) => MetaVarMap (UType binder typeSig Foil.VoidS) -> HMType (UType binder typeSig) -> [MetaVar]
freeMetaVarsHM subst (MonoType type_) = freeMetaVars subst type_
freeMetaVarsHM subst (PolyType (TypeScheme _ type_)) = freeMetaVars subst type_

-- * Inference monad

-- | How 'generalizeHM' finds the unification variables to quantify.
data Generalization
  = -- | Quantify the variables whose level is deeper than the current one
    -- (Rémy). This does not look at the typing environment.
    LevelBased
  | -- | Quantify the variables that do not occur in the typing environment
    -- (Damas and Milner). This traverses the whole environment at every @let@.
    Naive
  deriving (Eq, Show)

data TypingContext ty n = TypingContext
  { -- | Triangular substitution of unification variables.
    tcSubst :: MetaVarMap (ty Foil.VoidS),
    -- | Types of the variables in scope.
    tcTypings :: Foil.NameMap n (HMType ty),
    -- | Next unification variable.
    tcFreshId :: MetaVar,
    -- | Level of each unification variable that is not bound by the substitution.
    -- Invariant: a variable occurs in the typing environment (after applying
    -- the substitution) only if its level is at most the current level.
    tcLevels :: MetaVarMap Level,
    -- | Current level: the number of enclosing 'generalizeHM's.
    tcLevel :: Level,
    tcGeneralization :: Generalization
  }

initialTypingContext :: Generalization -> TypingContext ty Foil.VoidS
initialTypingContext generalization =
  TypingContext
    { tcSubst = emptyMetaVarMap,
      tcTypings = Foil.emptyNameMap,
      tcFreshId = MetaVar 0,
      tcLevels = emptyMetaVarMap,
      tcLevel = outermostLevel,
      tcGeneralization = generalization
    }

emptyTypingContext :: TypingContext ty Foil.VoidS
emptyTypingContext = initialTypingContext LevelBased

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

-- | A computation that infers the type of a child term.
type Infer typeBinder typeSig n =
  TypeCheck (UType typeBinder typeSig) n (UType typeBinder typeSig Foil.VoidS)

-- | A scoped child term (a pattern and the body in its scope), as a typing
-- rule sees it. As with @CheckInfer@ in the System F engine
-- ("FreeFoilTypecheck.SystemF.TypecheckGen"), a rule receives the child as a
-- record of operations.
data ScopedInfer typeBinder typeSig n = ScopedInfer
  { -- | Check the pattern against a type (see 'HMTypingPattern'), and return
    -- the types of the variables it binds, in the order of the pattern.
    checkBinderHM ::
      UType typeBinder typeSig Foil.VoidS ->
      TypeCheck (UType typeBinder typeSig) n [UType typeBinder typeSig Foil.VoidS],
    -- | Infer the type of the body, given the types of the variables bound
    -- by the pattern, in the order of the pattern.
    inferBodyHM :: [HMType (UType typeBinder typeSig)] -> Infer typeBinder typeSig n
  }

-- | Typing rules of a language, one per node of its term signature @sig@.
--
-- A rule receives the children of a node as computations, not as their types.
-- This way a rule decides when (and in which context) each child is inferred.
-- For example, the rule for @let@ infers the bound term inside 'generalizeHM',
-- and the rule for @λ@ chooses the type of the bound variable.
class HMTypingSig (binder :: Foil.S -> Foil.S -> K.Type) (typeSig :: K.Type -> K.Type -> K.Type) (sig :: K.Type -> K.Type -> K.Type) where
  inferSigHM ::
    sig (ScopedInfer binder typeSig n) (Infer binder typeSig n) ->
    Infer binder typeSig n

-- | Typing rules of the patterns of a language (the binders of its terms).
--
-- @checkPatternHM pattern type_@ checks that @pattern@ matches values of type
-- @type_@, and returns the types of the variables bound by @pattern@, in the
-- order of the pattern ('Foil.nameBinderListOf'). For example, a rule for a
-- pair pattern @(p, q)@ unifies @type_@ with a product of two fresh types and
-- checks @p@ and @q@ against them.
--
-- The class plays the role of @TypedPattern@ by Diana Tomilovskaia in the
-- System F engine ("FreeFoilTypecheck.SystemF.TypecheckGen"), which gives the
-- types of the variables of a pattern from the type of the pattern. Here the
-- check runs in 'TypeCheck', so it can unify types and reject a pattern that
-- does not match.
class HMTypingPattern (typeBinder :: Foil.S -> Foil.S -> K.Type) (typeSig :: K.Type -> K.Type -> K.Type) (binder :: Foil.S -> Foil.S -> K.Type) where
  checkPatternHM ::
    binder n l ->
    UType typeBinder typeSig Foil.VoidS ->
    TypeCheck (UType typeBinder typeSig) m [UType typeBinder typeSig Foil.VoidS]

-- * Operations for typing rules

-- | A fresh unification variable (at the current level).
freshMetaVar :: TypeCheck ty n MetaVar
freshMetaVar = do
  ctx <- get
  let x = tcFreshId ctx
  put
    ctx
      { tcFreshId = nextMetaVar x,
        tcLevels = insertMetaVarMap x (tcLevel ctx) (tcLevels ctx)
      }
  return x

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
    walk subst type_@(toMetaVar -> Just x) = maybe type_ (walk subst) (lookupMetaVarMap x subst)
    walk _ type_ = type_

-- | Bind a unification variable (after the occurs check).
-- With 'LevelBased' generalisation, the variables of the type
-- get the level of the bound variable, if theirs is deeper.
bindMetaVar :: (Bifoldable typeSig) => MetaVar -> UType binder typeSig Foil.VoidS -> TypeCheck (UType binder typeSig) n ()
bindMetaVar x type_ = do
  ctx <- get
  let vars = freeMetaVars (tcSubst ctx) type_
      levels = case (tcGeneralization ctx, lookupMetaVarMap x (tcLevels ctx)) of
        (LevelBased, Just level) -> foldr (adjustMetaVarMap (min level)) (tcLevels ctx) vars
        _ -> tcLevels ctx
  if x `elem` vars
    then failTypeCheck "occurs check failed"
    else put ctx {tcSubst = insertMetaVarMap x type_ (tcSubst ctx), tcLevels = levels}

-- | Infer the type of a child term one level deeper, and generalise it over
-- the unification variables that cannot occur in the typing environment
-- (see 'Generalization').
generalizeHM ::
  (Bifunctor typeSig, Bifoldable typeSig, Foil.CoSinkable binder) =>
  Infer binder typeSig n ->
  TypeCheck (UType binder typeSig) n (HMType (UType binder typeSig))
generalizeHM infer = do
  type_ <- enterLevel infer
  ctx <- get
  let subst = tcSubst ctx
      type' = zonk subst type_
      candidates = metaVarsOf type'
      vars = case tcGeneralization ctx of
        LevelBased ->
          [x | x <- candidates, findWithDefaultMetaVarMap outermostLevel x (tcLevels ctx) > tcLevel ctx]
        Naive ->
          let envVars = fromListMetaVarSet (concatMap (freeMetaVarsHM subst) (F.toList (tcTypings ctx)))
           in [x | x <- candidates, not (memberMetaVarSet x envVars)]
  return (generalize vars type')

-- | Like 'generalizeHM', for the types of the variables of a pattern, as
-- returned by a computation such as @bound >>= checkBinderHM body@. Each type
-- is generalised separately.
generalizePatternHM ::
  (Bifunctor typeSig, Bifoldable typeSig, Foil.CoSinkable binder) =>
  TypeCheck (UType binder typeSig) n [UType binder typeSig Foil.VoidS] ->
  TypeCheck (UType binder typeSig) n [HMType (UType binder typeSig)]
generalizePatternHM infer = do
  types <- enterLevel infer
  -- @generalizeHM (return t)@ generalises @t@ at the current level
  mapM (generalizeHM . return) types

-- | Run a computation one level deeper.
enterLevel :: TypeCheck ty n a -> TypeCheck ty n a
enterLevel action = do
  ctx <- get
  put ctx {tcLevel = deeperLevel (tcLevel ctx)}
  x <- action
  ctx' <- get
  put ctx' {tcLevel = shallowerLevel (tcLevel ctx')}
  return x

-- | Infer the type of a child term and unify it with the expected type.
checkHM ::
  (FreeFoil.ZipMatch typeSig, Bitraversable typeSig, Foil.CoSinkable binder) =>
  Infer binder typeSig n ->
  UType binder typeSig Foil.VoidS ->
  TypeCheck (UType binder typeSig) n ()
checkHM infer expected = do
  type_ <- infer
  unifyHM type_ expected

-- | Check the pattern of a scoped child term against a type, and infer the
-- type of the body with the (monomorphic) types of the variables of the
-- pattern. This is the rule for the binder of @λ@, for example.
inferScopedHM ::
  ScopedInfer binder typeSig n ->
  UType binder typeSig Foil.VoidS ->
  Infer binder typeSig n
inferScopedHM scoped type_ = do
  types <- checkBinderHM scoped type_
  inferBodyHM scoped (map MonoType types)

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
  (Foil.CoSinkable typeBinder, Bifunctor sig, Foil.CoSinkable binder, HMTypingSig typeBinder typeSig sig, HMTypingPattern typeBinder typeSig binder, Bifunctor typeSig) =>
  FreeFoil.AST binder sig Foil.VoidS ->
  TypeCheck (UType typeBinder typeSig) Foil.VoidS (UType typeBinder typeSig Foil.VoidS)
inferTypeNewClosed expr = reconstructType expr >>= zonkHM

-- | Infer the principal type scheme of a closed term.
inferTypeSchemeClosed ::
  (Foil.CoSinkable typeBinder, Bifunctor sig, Foil.CoSinkable binder, HMTypingSig typeBinder typeSig sig, HMTypingPattern typeBinder typeSig binder, Bifunctor typeSig, Bifoldable typeSig) =>
  Generalization ->
  FreeFoil.AST binder sig Foil.VoidS ->
  Either String (HMType (UType typeBinder typeSig))
inferTypeSchemeClosed generalization expr =
  fst <$> runTypeCheck (generalizeHM (reconstructType expr)) (initialTypingContext generalization)

-- | Infer the type of a term: instantiate the type of a variable,
-- or apply the typing rule of a node to its (suspended) children.
reconstructType ::
  (Foil.CoSinkable typeBinder, Bifunctor sig, Foil.CoSinkable binder, Bifunctor typeSig, HMTypingSig typeBinder typeSig sig, HMTypingPattern typeBinder typeSig binder) =>
  FreeFoil.AST binder sig n ->
  Infer typeBinder typeSig n
reconstructType = \case
  FreeFoil.Var x -> do
    ctx <- get
    instantiateHM (Foil.lookupName x (tcTypings ctx))
  FreeFoil.Node node ->
    inferSigHM (bimap reconstructTypeScoped reconstructType node)

reconstructTypeScoped ::
  (Foil.CoSinkable typeBinder, Bifunctor sig, Foil.CoSinkable binder, Bifunctor typeSig, HMTypingSig typeBinder typeSig sig, HMTypingPattern typeBinder typeSig binder) =>
  FreeFoil.ScopedAST binder sig n ->
  ScopedInfer typeBinder typeSig n
reconstructTypeScoped (FreeFoil.ScopedAST binder body) =
  ScopedInfer
    { checkBinderHM = checkPatternHM binder,
      inferBodyHM = \types -> enterScope binder types (reconstructType body)
    }

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
