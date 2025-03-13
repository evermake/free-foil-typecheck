{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-simplifiable-class-constraints #-}

module FreeFoilTypecheck.HindleyMilner.Inference where

import Control.Applicative (Const)
import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Free.Foil as Foil
import qualified Control.Monad.Free.Foil as FreeFoil
import Data.Bifunctor
import Data.Functor.Const (Const (..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified FreeFoilTypecheck.HindleyMilner.Parser.Abs as Raw
import FreeFoilTypecheck.HindleyMilner.Syntax

-- $setup
-- >>> :set -XOverloadedStrings

newtype Constraint = Constraint (Type', Type')

newtype Subst n = Subst (Map.Map Raw.UVarIdent (Type n))

class Typed a where
  applySubst :: (Foil.Distinct n) => Subst n -> a n -> a n

  -- | Returns a set of all free (unification) variables of a Typed.
  freeVars :: a n -> Set.Set Raw.UVarIdent

  -- | Returns whether a free (unification) variable occurs in a Typed.
  checkOccurs :: Raw.UVarIdent -> a n -> Bool
  checkOccurs i t = i `Set.member` freeVars t

instance Typed (Foil.AST FoilTypePattern TypeSig) where
  applySubst :: (Foil.Distinct n) => Subst n -> Type n -> Type n
  applySubst (Subst s) t = foldr applySubstToType t (Map.toList s)

  freeVars :: Type n -> Set.Set Raw.UVarIdent
  freeVars (TUVar ident) = Set.singleton ident
  freeVars (FreeFoil.Var _) = Set.empty
  freeVars (FreeFoil.Node node) = foldl (\idents typ -> Set.union idents (freeVars typ)) Set.empty node

instance Typed Subst where
  applySubst = undefined
  freeVars = undefined

instance Typed (Const Constraint) where
  applySubst s (Const (Constraint (t1, t2))) = Const (Constraint (applySubst s t1, applySubst s t2))
  freeVars (Const (Constraint (t1, t2))) = Set.union (freeVars t1) (freeVars t2)

applySubstToType :: (Foil.Distinct n) => (Raw.UVarIdent, Type n) -> Type n -> Type n
applySubstToType (ident, type_) (TUVar x)
  | ident == x = type_
  | otherwise = TUVar x
applySubstToType _ (FreeFoil.Var x) = FreeFoil.Var x
applySubstToType subst (FreeFoil.Node node) =
  FreeFoil.Node (bimap (applySubstToScopedType subst) (applySubstToType subst) node)

applySubstToScopedType :: (Foil.Distinct n) => (Raw.UVarIdent, Type n) -> FreeFoil.ScopedAST FoilTypePattern TypeSig n -> FreeFoil.ScopedAST FoilTypePattern TypeSig n
applySubstToScopedType subst' (FreeFoil.ScopedAST binder body) =
  case (Foil.assertExt binder, Foil.assertDistinct binder) of
    (Foil.Ext, Foil.Distinct) ->
      FreeFoil.ScopedAST binder (applySubstToType (fmap Foil.sink subst') body)
