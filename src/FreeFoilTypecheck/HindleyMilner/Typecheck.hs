{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-simplifiable-class-constraints #-}

module FreeFoilTypecheck.HindleyMilner.Typecheck where

-- import Control.Applicative (Const)
import Control.Monad (ap)
import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Foil as FreeFoil
import qualified Control.Monad.Foil.Internal as Foil
import qualified Control.Monad.Free.Foil as FreeFoil
import Data.Bifunctor
import qualified Data.Foldable as F
import qualified Data.IntMap as IntMap
import qualified FreeFoilTypecheck.HindleyMilner.Parser.Abs as Raw
import FreeFoilTypecheck.HindleyMilner.Syntax

-- $setup
-- >>> :set -XOverloadedStrings

-- >>> inferTypeNewClosed "λx. x"
-- Right ?u0 -> ?u0
-- >>> inferTypeNewClosed "λx. x + 1"
-- Right Nat -> Nat
-- >>> inferTypeNewClosed "let f = (λx. λy. let g = x y in g) in f (λz. z) 0"
-- Right Nat
-- >>> inferTypeNewClosed "let twice = (λt. (λx. (t (t x)))) in let add2 = (λx. x + 2) in let bool2int = (λb. if b then 1 else 0) in let not = (λb. if b then false else true) in (twice add2) (bool2int ((twice not) true))"
-- Right Nat
inferTypeNewClosed :: Exp Foil.VoidS -> Either String Type'
inferTypeNewClosed expr = do
  (type', TypingContext constrs substs _ _) <- runTypeCheck (reconstructType expr) (TypingContext [] [] Foil.emptyNameMap 0)
  substs' <- unify (map (applySubstsToConstraint substs) constrs)
  return (applySubstsToType substs' type')

type Constraint = (Type', Type')

type USubst n = (Raw.UVarIdent, Type n)

type USubst' = USubst Foil.VoidS

unify1 :: Constraint -> Either String [USubst']
unify1 c =
  case c of
    -- Case for unification variables
    (TUVar x, r) -> return [(x, r)]
    (l, TUVar x) -> return [(x, l)]
    -- Case for Free Foil variables (not supported for now)
    (FreeFoil.Var x, FreeFoil.Var y)
      | x == y -> return []
    -- Case of non-trivial arbitrary nodes
    (FreeFoil.Node l, FreeFoil.Node r) ->
      -- zipMatch (TArrowSig x1 x2) (TArrowSig y1 y2)
      --   = Just (TArrowSig (x1, y1) (x2, y2))
      case FreeFoil.zipMatch l r of
        Nothing -> Left ("cannot unify " ++ show c)
        -- `zipMatch` takes out corresponding terms from a node that we need
        --  to unify further.
        Just lr -> unify (F.toList lr) -- ignores "scopes", only works with "terms"
    (lhs, rhs) -> Left ("cannot unify " ++ show lhs ++ show rhs)

infixr 6 +++

(+++) :: [USubst'] -> [USubst'] -> [USubst']
xs +++ ys = map (applySubstsInSubsts ys) xs ++ ys

unify :: [Constraint] -> Either String [USubst']
unify [] = return []
unify (c : cs) = do
  substs <- unify1 c
  substs' <- unify (map (applySubstsToConstraint substs) cs)
  return (substs +++ substs')

unifyWith :: [USubst'] -> [Constraint] -> Either String [USubst']
unifyWith substs constraints = unify (map (applySubstsToConstraint substs) constraints)

newtype TypeCheck n a = TypeCheck {runTypeCheck :: TypingContext n -> Either String (a, TypingContext n)}
  deriving (Functor)

-- instance Functor (TypeCheck n) where
--   fmap f (TypeCheck g) = TypeCheck $ \tc ->
--     case g tc of
--       Left err -> Left err
--       Right (x, tc') -> Right (f x, tc')

instance Applicative (TypeCheck n) where
  pure x = TypeCheck $ \tc -> Right (x, tc)
  (<*>) = ap

instance Monad (TypeCheck n) where
  -- return x = TypeCheck $ \tc -> Right (x, tc)

  -- (>>=) :: TypeCheck a -> (a -> TypeCheck b) -> TypeCheck b
  -- g :: TypingContext n -> Either String (a, TypingContext n)
  -- TypeCheck g >>= f = TypeCheck $ \tc ->
  --   case g tc of
  --     Left err -> Left err
  --     Right (x, tc') -> runTypeCheck (f x) tc'
  --
  -- do
  --  x <- TypeCheck g
  --  f x
  TypeCheck g >>= f = TypeCheck $ \tc -> do
    (x, tc') <- g tc
    runTypeCheck (f x) tc'

applySubstsToConstraint :: [USubst'] -> Constraint -> Constraint
applySubstsToConstraint substs (l, r) = (applySubstsToType substs l, applySubstsToType substs r)

applySubstToType :: (Foil.Distinct n) => USubst n -> Type n -> Type n
applySubstToType (ident, typ) (TUVar x)
  | ident == x = typ
  | otherwise = TUVar x
applySubstToType _ (FreeFoil.Var x) = FreeFoil.Var x
applySubstToType subst (FreeFoil.Node node) =
  FreeFoil.Node (bimap (applySubstToScopedType subst) (applySubstToType subst) node)
  where
    applySubstToScopedType :: (Foil.Distinct n) => USubst n -> FreeFoil.ScopedAST FoilTypePattern TypeSig n -> FreeFoil.ScopedAST FoilTypePattern TypeSig n
    applySubstToScopedType subst' (FreeFoil.ScopedAST binder body) =
      case (Foil.assertExt binder, Foil.assertDistinct binder) of
        (Foil.Ext, Foil.Distinct) ->
          FreeFoil.ScopedAST binder (applySubstToType (fmap Foil.sink subst') body)

applySubstsToType :: [USubst'] -> Type' -> Type'
applySubstsToType [] typ = typ
applySubstsToType (subst : rest) typ = applySubstsToType rest (applySubstToType subst typ)

applySubstsInSubsts :: [USubst'] -> USubst' -> USubst'
applySubstsInSubsts substs (l, r) = (l, (applySubstsToType substs r))

deriving instance Functor (Foil.NameMap n)

deriving instance Foldable (Foil.NameMap n)

data TypingContext n = TypingContext
  { tcConstraints :: [Constraint],
    tcSubsts :: [USubst'],
    tcTypings :: FreeFoil.NameMap n Type',
    tcFreshId :: Int
  }

get :: TypeCheck n (TypingContext n)
get = TypeCheck $ \tc -> Right (tc, tc)

put :: TypingContext n -> TypeCheck n ()
put new = TypeCheck $ \_old -> Right ((), new)

eitherToTypeCheck :: Either String a -> TypeCheck n a
eitherToTypeCheck (Left err) = TypeCheck $ \_tc -> Left err
eitherToTypeCheck (Right x) = TypeCheck $ \tc -> Right (x, tc)

unifyTypeCheck :: TypeCheck n ()
unifyTypeCheck = do
  TypingContext constraints substs ctx freshId <- get
  substs' <- eitherToTypeCheck (unifyWith substs constraints)
  put (TypingContext [] (substs +++ substs') ctx freshId)

enterScope :: Foil.NameBinder n l -> Type' -> TypeCheck l a -> TypeCheck n a
enterScope binder type_ code = do
  TypingContext constraints substs ctx freshId <- get
  let ctx' = Foil.addNameBinder binder type_ ctx
  (x, TypingContext constraints'' substs'' ctx'' freshId'') <-
    eitherToTypeCheck $
      runTypeCheck code (TypingContext constraints substs ctx' freshId)
  let ctx''' = popNameBinder binder ctx''
  put (TypingContext constraints'' substs'' ctx''' freshId'')
  return x

addConstraints :: [Constraint] -> TypeCheck n ()
addConstraints constrs = do
  TypingContext constraints substs ctx freshId <- get
  put (TypingContext (constrs ++ constraints) substs ctx freshId)

freshTypeVar :: TypeCheck n Type'
freshTypeVar = do
  TypingContext constraints substs ctx freshId <- get
  put (TypingContext constraints substs ctx (freshId + 1))
  return (TUVar (makeIdent freshId))

-- | Recursively "reconstructs" type of an expression.
-- On success, returns the "reconstructed" type and collected constraints.
reconstructType :: Exp n -> TypeCheck n Type'
reconstructType ETrue = return TBool
reconstructType EFalse = return TBool
reconstructType (ENat _) = return TNat -- TypeCheck $ \tc -> Right (TNat, tc)
reconstructType (FreeFoil.Var x) = do
  TypingContext constrs subst ctx freshId <- get
  let xTyp = Foil.lookupName x ctx
  let (specTyp, freshId2) = specialize xTyp freshId
  put (TypingContext constrs subst ctx freshId2)
  return specTyp
reconstructType (ELet eWhat (FoilPatternVar x) eExpr) = do
  whatTyp <- reconstructType eWhat
  unifyTypeCheck
  (TypingContext _ substs ctx _) <- get
  let whatTyp1 = applySubstsToType substs whatTyp
  let ctx' = fmap (applySubstsToType substs) ctx
  let ctxVars = foldl (\idents typ -> idents ++ allUVarsOfType typ) [] ctx'
  let whatFreeIdents = filter (\i -> not (elem i ctxVars)) (allUVarsOfType whatTyp1)
  let whatTyp2 = generalize whatFreeIdents whatTyp1
  enterScope x whatTyp2 (reconstructType eExpr)
reconstructType (EAdd lhs rhs) = do
  lhsTyp <- reconstructType lhs
  rhsTyp <- reconstructType rhs
  addConstraints [(lhsTyp, TNat), (rhsTyp, TNat)]
  return TNat
reconstructType (ESub lhs rhs) = do
  lhsTyp <- reconstructType lhs
  rhsTyp <- reconstructType rhs
  addConstraints [(lhsTyp, TNat), (rhsTyp, TNat)]
  return TNat
reconstructType (EIf eCond eThen eElse) = do
  condTyp <- reconstructType eCond
  thenTyp <- reconstructType eThen
  elseTyp <- reconstructType eElse
  addConstraints [(condTyp, TBool), (thenTyp, elseTyp)]
  return thenTyp
reconstructType (EIsZero e) = do
  eTyp <- reconstructType e
  addConstraints [(eTyp, TNat)]
  return TBool
reconstructType (EAbs (FoilPatternVar x) eBody) = do
  paramType <- freshTypeVar
  bodyTyp <-
    enterScope x paramType $
      reconstructType eBody
  return (TArrow paramType bodyTyp)
reconstructType (EApp eAbs eArg) = do
  absTyp <- reconstructType eAbs
  argTyp <- reconstructType eArg
  resultTyp <- freshTypeVar
  addConstraints [(absTyp, TArrow argTyp resultTyp)]
  return resultTyp
reconstructType (ETyped e typ_) = do
  let typ = toTypeClosed typ_
  eTyp <- reconstructType e
  addConstraints [(eTyp, typ)]
  return typ
reconstructType (EFor eFrom eTo (FoilPatternVar x) eBody) = do
  fromTyp <- reconstructType eFrom
  toTyp <- reconstructType eTo
  addConstraints [(fromTyp, TNat), (toTyp, TNat)]
  enterScope x TNat $
    reconstructType eBody

allUVarsOfType :: Type' -> [Raw.UVarIdent]
allUVarsOfType (TUVar ident) = [ident]
allUVarsOfType (FreeFoil.Var _) = []
allUVarsOfType (FreeFoil.Node node) = foldl (\idents typ -> idents ++ allUVarsOfType typ) [] node

popNameBinder :: Foil.NameBinder n l -> Foil.NameMap l a -> Foil.NameMap n a
popNameBinder name (Foil.NameMap m) = Foil.NameMap (IntMap.delete (Foil.nameId (Foil.nameOf name)) m)

unificationVarIdentsBetween :: Int -> Int -> [Raw.UVarIdent]
unificationVarIdentsBetween a b = map makeIdent [a .. (b - 1)]

makeIdent :: Int -> Raw.UVarIdent
makeIdent i = Raw.UVarIdent ("?u" ++ (show i))

-- >>> generalize ["?a", "?b"] "?a -> ?b -> ?a"
-- forall x0 . forall x1 . x0 -> x1 -> x0
-- >>> generalize ["?b", "?a"] "?a -> ?b -> ?a"
-- forall x0 . forall x1 . x1 -> x0 -> x1
generalize :: [Raw.UVarIdent] -> Type' -> Type'
generalize = go Foil.emptyScope
  where
    go :: (Foil.Distinct n) => Foil.Scope n -> [Raw.UVarIdent] -> Type n -> Type n
    go _ [] type_ = type_
    go ctx (x : xs) type_ = Foil.withFresh ctx $ \binder ->
      let newScope = Foil.extendScope binder ctx
          x' = FreeFoil.Var (Foil.nameOf binder)
          type' = applySubstToType (x, x') (Foil.sink type_)
       in TForAll (FoilTPatternVar binder) (go newScope xs type')

-- addSubst
--   :: forall e i o i'. Substitution e i o
--   -> NameBinder i i'
--   -> e o
--   -> Substitution e i' o

-- binder :: NameBinder VoidS l0

-- addSubst identitySubst :: NameBinder io i' -> e io -> Substitution e i' io
-- addSubst identitySubst binder :: e VoidS -> Substitution e l0 VoidS
-- addSubst identitySubst binder ... :: Substitution e l0 VoidS

-- >>> specialize "forall a. forall b. a -> b" 6
-- (?u6 -> ?u7,8)
specialize :: Type' -> Int -> (Type', Int)
specialize (TForAll (FoilTPatternVar binder) type_) freshId =
  let subst = Foil.addSubst Foil.identitySubst binder (TUVar (makeIdent freshId))
   in specialize (FreeFoil.substitute Foil.emptyScope subst type_) (freshId + 1)
specialize type_ freshId = (type_, freshId)
