{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-simplifiable-class-constraints #-}

module HM.TestFreeFoil where

-- import qualified Control.Monad.Foil as Foil
-- import qualified Control.Monad.Foil.Internal as Foil
import qualified Control.Monad.Free.Foil as FreeFoil
import Data.Bifoldable (bisum)
import Data.Bifunctor
import Data.Bits
-- import qualified Data.Foldable as F
-- import qualified Data.Map.Internal.Debug as FreeFoil
-- import qualified HM.Parser.Abs as Raw
import HM.Syntax

sizeOfType :: Type n -> Int
sizeOfType TNat = finiteBitSize (1 :: Int)
sizeOfType TBool = finiteBitSize (True :: Bool)
sizeOfType (TArrow lhs rhs) = sizeOfType lhs + sizeOfType rhs
sizeOfType (TUVar _) = 1
sizeOfType (FreeFoil.Node n) = 1 + bisum (bimap sizeOfScopedType sizeOfType n)
  where
    sizeOfScopedType (FreeFoil.ScopedAST _ body) = sizeOfType body
sizeOfType (FreeFoil.Var _) = 1

sizeOfExp :: Exp n -> Int
sizeOfExp (FreeFoil.Var _) = 1
sizeOfExp (FreeFoil.Node node) = 1 + bisum (bimap sizeOfScopedExp sizeOfExp node)
  where
    sizeOfScopedExp (FreeFoil.ScopedAST _ body) = sizeOfExp body

-- sizeOfAST :: (Bifunctor sig) => AST sig n -> Int

-- sizeOfAST body = r