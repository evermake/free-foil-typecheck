{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-simplifiable-class-constraints #-}

module HM.Typecheck where

import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Foil.Internal as Foil
import qualified Control.Monad.Free.Foil as FreeFoil
import Data.Bifunctor
import qualified Data.Foldable as F
import qualified HM.Parser.Abs as Raw
import HM.Syntax

-- $setup
-- >>> :set -XOverloadedStrings

-- >>> inferTypeNewClosed "λx. λy. x y"
-- Right ?u1 -> ?u2 -> ?u1 -> ?u2
inferTypeNewClosed :: Exp Foil.VoidS -> Either String Type'
inferTypeNewClosed expr = do
  (type', (constrs, _, _)) <- reconstructType ([], Foil.emptyNameMap, 0) expr
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
        Nothing -> Left "cannot unify"
        -- `zipMatch` takes out corresponding terms from a node that we need
        --  to unify further.
        Just lr -> unify (F.toList lr) -- ignores "scopes", only works with "terms"
    _ -> Left "cannot unify"

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

-- | Recursively "reconstructs" type of an expression.
-- On success, returns the "reconstructed" type and collected constraints.
--
-- >>> reconstructType [] 1 Foil.emptyNameMap "λx. λy. x y"
-- Right (?u1 -> ?u2 -> ?u3,[(?u1,?u2 -> ?u3)],4)
--
-- >>> reconstructType [] 1 Foil.emptyNameMap "(λx. λy. (let g = (x y) in g))"
-- Right (?u1 -> ?u2 -> ?u4,[],5)
reconstructType :: ([Constraint], Foil.NameMap n Type', Int) -> Exp n -> Either String (Type', ([Constraint], Foil.NameMap n Type', Int))
reconstructType arg ETrue = Right (TBool, arg)
reconstructType arg EFalse = Right (TBool, arg)
reconstructType arg (ENat _) = Right (TNat, arg)
reconstructType (constrs, ctx, freshId) (FreeFoil.Var x) = do
  let xTyp = Foil.lookupName x ctx
  let (specTyp, freshId2) = specialize xTyp freshId
  return (specTyp, (constrs, ctx, freshId2))
reconstructType (constrs, ctx, freshId) (ELet eWhat x eExpr) = do
  (whatTyp, (constrs2, ctx2, freshId2)) <- reconstructType (constrs, ctx, freshId) eWhat
  substs <- unify constrs2
  let whatTyp1 = applySubstsToType substs whatTyp
  let ctx3 = fmap (applySubstsToType substs) ctx2
  let ctxVars = foldl (\idents typ -> idents ++ allUVarsOfType typ) [] ctx3
  let whatFreeIdents = filter (\i -> elem i ctxVars) (allUVarsOfType whatTyp1)
  let whatTyp2 = generalize whatFreeIdents whatTyp1
  let ctx4 = Foil.addNameBinder x whatTyp2 ctx3
  -- Since we've unified everything, new constraints are empty.
  (exprTyp, (constrs3, ctx5, freshId3)) <- reconstructType ([], ctx4, freshId2) eExpr
  return (exprTyp, (constrs3, ctx5, freshId3))
reconstructType (constrs, ctx, freshId) (EAdd lhs rhs) = do
  (lhsTyp, (constrs2, ctx2, freshId2)) <- reconstructType (constrs, ctx, freshId) lhs
  (rhsTyp, (constrs3, ctx3, freshId3)) <- reconstructType (constrs2, ctx2, freshId2) rhs
  return (TNat, (constrs3 ++ [(lhsTyp, TNat), (rhsTyp, TNat)], ctx3, freshId3))
reconstructType (constrs, ctx, freshId) (ESub lhs rhs) = do
  (lhsTyp, (constrs2, ctx2, freshId2)) <- reconstructType (constrs, ctx, freshId) lhs
  (rhsTyp, (constrs3, ctx3, freshId3)) <- reconstructType (constrs2, ctx2, freshId2) rhs
  return (TNat, (constrs3 ++ [(lhsTyp, TNat), (rhsTyp, TNat)], ctx3, freshId3))
reconstructType (constrs, ctx, freshId) (EIf eCond eThen eElse) = do
  (condTyp, (constrs2, ctx2, freshId2)) <- reconstructType (constrs, ctx, freshId) eCond
  (thenTyp, (constrs3, ctx3, freshId3)) <- reconstructType (constrs2, ctx2, freshId2) eThen
  (elseTyp, (constrs4, ctx4, freshId4)) <- reconstructType (constrs3, ctx3, freshId3) eElse
  return (thenTyp, (constrs4 ++ [(condTyp, TBool), (thenTyp, elseTyp)], ctx4, freshId4))
reconstructType (constrs, ctx, freshId) (EIsZero e) = do
  (eTyp, (constrs2, ctx2, freshId2)) <- reconstructType (constrs, ctx, freshId) e
  return (TBool, (constrs2 ++ [(eTyp, TNat)], ctx2, freshId2))
reconstructType (constrs, ctx, freshId) (EAbs x eBody) = do
  let paramType = TUVar (makeIdent freshId)
  let ctx2 = Foil.addNameBinder x paramType ctx
  (bodyTyp, (constrs2, ctx3, freshId2)) <- reconstructType (constrs, ctx2, (freshId + 1)) eBody
  return (TArrow paramType bodyTyp, (constrs2, ctx3, freshId2))
reconstructType (constrs, ctx, freshId) (EApp eAbs eArg) = do
  (absTyp, (constrs2, ctx2, freshId2)) <- reconstructType (constrs, ctx, freshId) eAbs
  (argTyp, (constrs3, ctx3, freshId3)) <- reconstructType (constrs2, ctx2, freshId2) eArg
  let resultTyp = TUVar (makeIdent freshId3)
  return (resultTyp, (constrs3 ++ [(absTyp, (TArrow argTyp resultTyp))], ctx3, freshId3 + 1))
reconstructType (constrs, ctx, freshId) (ETyped e typ_) = do
  let typ = toTypeClosed typ_
  (eTyp, (constrs2, ctx2, freshId2)) <- reconstructType (constrs, ctx, freshId) e
  return (typ, (constrs2 ++ [(eTyp, typ)], ctx2, freshId2))
reconstructType (constrs, ctx, freshId) (EFor eFrom eTo x eBody) = do
  (fromTyp, (constrs2, ctx2, freshId2)) <- reconstructType (constrs, ctx, freshId) eFrom
  (toTyp, (constrs3, ctx3, freshId3)) <- reconstructType (constrs2, ctx2, freshId2) eTo
  let ctx4 = Foil.addNameBinder x TNat ctx3
  (bodyTyp, (constrs4, ctx5, freshId4)) <- reconstructType (constrs3, ctx4, freshId3) eBody
  return (bodyTyp, (constrs4 ++ [(fromTyp, TNat), (toTyp, TNat)], ctx5, freshId4))

allUVarsOfType :: Type' -> [Raw.UVarIdent]
allUVarsOfType (TUVar ident) = [ident]
allUVarsOfType (FreeFoil.Var _) = []
allUVarsOfType (FreeFoil.Node node) = foldl (\idents typ -> idents ++ allUVarsOfType typ) [] node

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
