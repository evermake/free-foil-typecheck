{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FreeFoilTypecheck.SystemF.FreeFoilExt where

import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Foil.Internal as Foil
import qualified Control.Monad.Free.Foil as FreeFoil
import Data.Bifoldable (Bifoldable (bifoldMap))
import qualified Data.IntMap as IntMap
import Data.Maybe (mapMaybe)
import Unsafe.Coerce (unsafeCoerce)

-- HELPERS

-- FIXME: should be part of free-foil
deriving instance Functor (Foil.NameMap n)

deriving instance Foldable (Foil.NameMap n)

deriving instance Traversable (Foil.NameMap n)

nameMapToScope :: Foil.NameMap n a -> Foil.Scope n
nameMapToScope (Foil.NameMap m) = Foil.UnsafeScope (IntMap.keysSet m)

unsinkAST :: (Foil.Distinct l, Foil.CoSinkable binder, Bifoldable sig) => Foil.Scope n -> FreeFoil.AST binder sig l -> Maybe (FreeFoil.AST binder sig n)
unsinkAST scope term
  | all (`Foil.member` scope) (freeVarsOf term) = Just (unsafeCoerce term)
  | otherwise = Nothing

freeVarsOf :: (Foil.Distinct n, Foil.CoSinkable binder, Bifoldable sig) => FreeFoil.AST binder sig n -> [Foil.Name n]
freeVarsOf = \case
  FreeFoil.Var name -> [name]
  FreeFoil.Node node -> bifoldMap freeVarsOfScopedAST freeVarsOf node

-- ΛY. λy:Y.  let z = y in ΛX. λa : X. z
-- Γ, Y, y : Y, z : Y    ⊢ ∀X. X → Y
freeVarsOfScopedAST :: (Foil.Distinct n, Foil.CoSinkable binder, Bifoldable sig) => FreeFoil.ScopedAST binder sig n -> [Foil.Name n]
freeVarsOfScopedAST (FreeFoil.ScopedAST binder body) =
  case Foil.assertDistinct binder of
    Foil.Distinct -> mapMaybe (Foil.unsinkNamePattern binder) (freeVarsOf body)


