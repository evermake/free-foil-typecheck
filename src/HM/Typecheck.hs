{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-simplifiable-class-constraints #-}

module HM.Typecheck where

-- import Control.Applicative (Const)
import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Foil as FreeFoil
import qualified Control.Monad.Foil.Internal as Foil
import qualified Control.Monad.Free.Foil as FreeFoil
import Data.Bifunctor
import qualified Data.Foldable as F
import qualified Data.IntMap as IntMap
import qualified HM.Parser.Abs as Raw
import HM.Syntax

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
  (type', TypingContext constrs _ _ _) <- reconstructType (TypingContext [] [] Foil.emptyNameMap 0) expr
  substs <- unify constrs
  return (applySubstsToType substs type')

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

unify :: [Constraint] -> Either String [USubst']
unify [] = return []
unify (c : cs) = do
  subst <- unify1 c
  subst' <- unify (map (applySubstsToConstraint subst) cs)
  return (map (applySubstsInSubsts subst') subst ++ subst')

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
    applySubstToScopedType :: (Foil.Distinct n) => USubst n -> FreeFoil.ScopedAST TypeSig n -> FreeFoil.ScopedAST TypeSig n
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

-- | Recursively "reconstructs" type of an expression.
-- On success, returns the "reconstructed" type and collected constraints.
--
-- >>> reconstructType [] 1 Foil.emptyNameMap "λx. λy. x y"
-- Right (?u1 -> ?u2 -> ?u3,[(?u1,?u2 -> ?u3)],4)
--
-- >>> reconstructType [] 1 Foil.emptyNameMap "(λx. λy. (let g = (x y) in g))"
-- Right (?u1 -> ?u2 -> ?u4,[],5)
reconstructType :: TypingContext n -> Exp n -> Either String (Type', TypingContext n)
reconstructType arg ETrue = Right (TBool, arg)
reconstructType arg EFalse = Right (TBool, arg)
reconstructType arg (ENat _) = Right (TNat, arg)
reconstructType (TypingContext constrs subst ctx freshId) (FreeFoil.Var x) = do
  let xTyp = Foil.lookupName x ctx
  let (specTyp, freshId2) = specialize xTyp freshId
  return (specTyp, TypingContext constrs subst ctx freshId2)
reconstructType tc (ELet eWhat x eExpr) = do
  (whatTyp, TypingContext constrs2 substs2 ctx2 freshId2) <- reconstructType tc eWhat
  substs <- unify constrs2
  let whatTyp1 = applySubstsToType substs whatTyp
  let ctx3 = fmap (applySubstsToType substs) ctx2
  let ctxVars = foldl (\idents typ -> idents ++ allUVarsOfType typ) [] ctx3
  let whatFreeIdents = filter (\i -> not (elem i ctxVars)) (allUVarsOfType whatTyp1)
  let whatTyp2 = generalize whatFreeIdents whatTyp1
  let ctx4 = Foil.addNameBinder x whatTyp2 ctx3
  (exprTyp, TypingContext constrs3 substs3 ctx5 freshId3) <- reconstructType (TypingContext [] substs ctx4 freshId2) eExpr -- DOESN'T work with empty constraints
  -- (exprTyp, TypingContext constrs3 substs3 ctx5 freshId3) <- reconstructType (TypingContext constrs2 substs ctx4 freshId2) eExpr -- DO work with previous constraints
  let ctx6 = popNameBinder x ctx5
  return (exprTyp, TypingContext constrs3 (map (applySubstsInSubsts substs2) substs3 ++ substs2) ctx6 freshId3)
reconstructType tc (EAdd lhs rhs) = do
  (lhsTyp, tc2) <- reconstructType tc lhs
  (rhsTyp, TypingContext constrs2 subst2 ctx2 freshId2) <- reconstructType tc2 rhs
  return (TNat, TypingContext (constrs2 ++ [(applySubstsToType subst2 lhsTyp, TNat), (rhsTyp, TNat)]) subst2 ctx2 freshId2)
reconstructType tc (ESub lhs rhs) = do
  (lhsTyp, tc2) <- reconstructType tc lhs
  (rhsTyp, TypingContext constrs2 subst2 ctx2 freshId2) <- reconstructType tc2 rhs
  return (TNat, TypingContext (constrs2 ++ [(applySubstsToType subst2 lhsTyp, TNat), (rhsTyp, TNat)]) subst2 ctx2 freshId2)
reconstructType tc (EIf eCond eThen eElse) = do
  (condTyp, tc2) <- reconstructType tc eCond
  (thenTyp, tc3) <- reconstructType tc2 eThen
  (elseTyp, TypingContext constrs3 subst3 ctx3 freshId3) <- reconstructType tc3 eElse
  return (thenTyp, TypingContext (constrs3 ++ [(applySubstsToType subst3 condTyp, TBool), (applySubstsToType subst3 thenTyp, elseTyp)]) subst3 ctx3 freshId3)
reconstructType tc (EIsZero e) = do
  (eTyp, TypingContext constrs subst ctx freshId) <- reconstructType tc e
  return (TBool, TypingContext (constrs ++ [(eTyp, TNat)]) subst ctx freshId)
reconstructType (TypingContext constrs subst ctx freshId) (EAbs x eBody) = do
  let paramType = TUVar (makeIdent freshId)
  let ctx2 = Foil.addNameBinder x paramType ctx
  (bodyTyp, TypingContext constrs2 subst2 ctx3 freshId2) <- reconstructType (TypingContext constrs subst ctx2 (freshId + 1)) eBody
  let ctx4 = popNameBinder x ctx3
  return (TArrow paramType bodyTyp, TypingContext constrs2 subst2 ctx4 freshId2)
reconstructType tc (EApp eAbs eArg) = do
  (absTyp, tc2) <- reconstructType tc eAbs
  (argTyp, TypingContext constrs3 subst3 ctx3 freshId3) <- reconstructType tc2 eArg
  let resultTyp = TUVar (makeIdent freshId3)
  return (resultTyp, TypingContext (constrs3 ++ [(applySubstsToType subst3 absTyp, TArrow argTyp resultTyp)]) subst3 ctx3 (freshId3 + 1))
reconstructType tc (ETyped e typ_) = do
  let typ = toTypeClosed typ_
  (eTyp, TypingContext constrs2 subst2 ctx2 freshId2) <- reconstructType tc e
  return (typ, TypingContext (constrs2 ++ [(eTyp, typ)]) subst2 ctx2 freshId2)
reconstructType tc (EFor eFrom eTo x eBody) = do
  (fromTyp, tc2) <- reconstructType tc eFrom
  (toTyp, TypingContext constrs3 subst3 ctx3 freshId3) <- reconstructType tc2 eTo
  let ctx4 = Foil.addNameBinder x TNat ctx3
  (bodyTyp, TypingContext constrs4 subst4 ctx5 freshId4) <- reconstructType (TypingContext constrs3 subst3 ctx4 freshId3) eBody
  let ctx6 = popNameBinder x ctx5
  return (bodyTyp, TypingContext (constrs4 ++ [(applySubstsToType subst4 fromTyp, TNat), (applySubstsToType subst4 toTyp, TNat)]) subst4 ctx6 freshId4)

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
       in TForAll binder (go newScope xs type')

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
specialize (TForAll binder type_) freshId =
  let subst = Foil.addSubst Foil.identitySubst binder (TUVar (makeIdent freshId))
   in specialize (FreeFoil.substitute Foil.emptyScope subst type_) (freshId + 1)
specialize type_ freshId = (type_, freshId)
