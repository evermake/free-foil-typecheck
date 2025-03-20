{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-simplifiable-class-constraints #-}

module FreeFoilTypeInferencer.HindleyMilner.Inference where

import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Foil.Internal as Foil
import qualified Control.Monad.Free.Foil as FreeFoil
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified FreeFoilTypecheck.HindleyMilner.Parser.Abs as Raw
import FreeFoilTypecheck.HindleyMilner.Syntax

-- FIXME: should be in the free-foil package.
deriving instance Functor (Foil.NameMap n)

deriving instance Foldable (Foil.NameMap n)

deriving instance (Show a) => Show (Foil.NameMap n a)

-- $setup
-- >>> :set -XOverloadedStrings

type IdentLevelMap = HashMap.HashMap Raw.UVarIdent Int

newtype Constraint n = Constraint (Type n, Type n)

type Constraint' = Constraint Foil.VoidS

newtype Subst n = Subst (Map.Map Raw.UVarIdent (Type n))

type Subst' = Subst Foil.VoidS

-- Question: how to swap type parameters but implement `Typed` below?
newtype TypingEnv n m = TypingEnv (Foil.NameMap m (Type n))

type TypingEnv' n = TypingEnv Foil.VoidS n

class Typed a where
  applySubst :: (Foil.Distinct n) => Subst n -> a n -> a n

  -- | Returns a set of all free (unification) variables of a Typed.
  freeVars :: a n -> Set.Set Raw.UVarIdent

  -- | Returns whether a free (unification) variable occurs in a Typed.
  hasFreeVar :: Raw.UVarIdent -> a n -> Bool
  hasFreeVar i t = i `Set.member` freeVars t

instance Typed (FreeFoil.AST FoilTypePattern TypeSig) where
  applySubst (Subst s) t = foldr applySubstToType t (Map.toList s)

  freeVars (TUVar ident) = Set.singleton ident
  freeVars (FreeFoil.Var _) = Set.empty
  freeVars (FreeFoil.Node node) = Set.unions $ freeVars <$> node

instance Typed Constraint where
  applySubst s (Constraint (t1, t2)) = Constraint (applySubst s t1, applySubst s t2)

  freeVars (Constraint (t1, t2)) = Set.union (freeVars t1) (freeVars t2)

instance Typed (TypingEnv Foil.VoidS) where
  -- FIXME: TypingEnv is a mapping from binder to type in the empty scope,
  --        but `applySubst` has `Subst` within scope `n` in type definition.
  applySubst s (TypingEnv env) = TypingEnv (fmap (applySubst s) env)

  freeVars (TypingEnv env) = Set.unions $ freeVars <$> env

composeSubst :: (Foil.Distinct n) => Subst n -> Subst n -> Subst n
composeSubst (Subst s1) (Subst s2) = Subst (Map.map (applySubst (Subst s2)) s1 `Map.union` s2)

applySubstToType :: (Foil.Distinct n) => (Raw.UVarIdent, Type n) -> Type n -> Type n
applySubstToType (ident, type_) (TUVar x)
  | ident == x = type_
  | otherwise = TUVar x
applySubstToType _ (FreeFoil.Var x) = FreeFoil.Var x
applySubstToType subst (FreeFoil.Node node) =
  FreeFoil.Node (bimap (applySubstToScopedType subst) (applySubstToType subst) node)

applySubstToScopedType :: (Foil.Distinct n) => (Raw.UVarIdent, Type n) -> FreeFoil.ScopedAST FoilTypePattern TypeSig n -> FreeFoil.ScopedAST FoilTypePattern TypeSig n
applySubstToScopedType subst (FreeFoil.ScopedAST binder body) =
  case (Foil.assertExt binder, Foil.assertDistinct binder) of
    (Foil.Ext, Foil.Distinct) ->
      FreeFoil.ScopedAST binder (applySubstToType (fmap Foil.sink subst) body)
