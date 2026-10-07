{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FreeFoilTypecheck.HindleyMilner.Syntax where

import qualified Control.Monad.Foil as Foil
import Control.Monad.Foil.TH
import Control.Monad.Free.Foil
import Control.Monad.Free.Foil.TH
import Data.Bifunctor.TH
import Data.Bifunctor.Sum(Sum(..))
import Data.Map (Map)
import qualified Data.Kind as K
import qualified Data.Map as Map
import Data.String (IsString (..))
import Data.ZipMatchK (ZipMatchK (..), zipMatchViaEq)
import Data.ZipMatchK.TH (deriveZipMatchK)
import Generics.Kind.TH (deriveGenericK)
import qualified FreeFoilTypecheck.HindleyMilner.Parser.Abs as Raw
import qualified FreeFoilTypecheck.HindleyMilner.Parser.Par as Raw
import qualified FreeFoilTypecheck.HindleyMilner.Parser.Print as Raw
import FreeFoilTypecheck.Orphans ()
import FreeFoilTypecheck.ScopeCheck (checkClosed)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XDataKinds
-- >>> import qualified Control.Monad.Foil as Foil
-- >>> import Control.Monad.Free.Foil
-- >>> import Data.String (fromString)
-- >>> import qualified FreeFoilTypecheck.HindleyMilner.Parser.Par as Raw

-- * Generated code (expressions)

-- ** Signature

mkSignature ''Raw.Exp ''Raw.Ident ''Raw.ScopedExp ''Raw.Pattern
deriveBifunctor ''ExpSig
deriveBifoldable ''ExpSig
deriveBitraversable ''ExpSig

-- | Matching two expressions compares their type annotations syntactically,
-- so annotations that differ only in the names of bound type variables
-- do not match.
instance ZipMatchK Raw.Type where
  zipMatchWithK = zipMatchViaEq

deriveZipMatchK ''ExpSig

-- ** Pattern synonyms

mkPatternSynonyms ''ExpSig

-- ** Conversion helpers

mkConvertToFreeFoil ''Raw.Exp ''Raw.Ident ''Raw.ScopedExp ''Raw.Pattern
mkConvertFromFreeFoil ''Raw.Exp ''Raw.Ident ''Raw.ScopedExp ''Raw.Pattern

-- ** Scope-safe patterns

mkFoilPattern ''Raw.Ident ''Raw.Pattern
deriveCoSinkable ''Raw.Ident ''Raw.Pattern
deriveGenericK ''FoilPattern
instance Foil.SinkableK FoilPattern
mkToFoilPattern ''Raw.Ident ''Raw.Pattern
mkFromFoilPattern ''Raw.Ident ''Raw.Pattern

instance Foil.UnifiablePattern FoilPattern where
  unifyPatterns (FoilPatternVar x) (FoilPatternVar y) = Foil.unifyNameBinders x y

-- * Generated code (types)

-- ** Signature

mkSignature ''Raw.Type ''Raw.Ident ''Raw.ScopedType ''Raw.TypePattern
deriveBifunctor ''TypeSig
deriveBifoldable ''TypeSig
deriveBitraversable ''TypeSig

-- | Matching two types compares the names of unification variables.
instance ZipMatchK Raw.UVarIdent where
  zipMatchWithK = zipMatchViaEq

deriveZipMatchK ''TypeSig

-- ** Pattern synonyms

mkPatternSynonyms ''TypeSig

-- ** Conversion helpers

mkConvertToFreeFoil ''Raw.Type ''Raw.Ident ''Raw.ScopedType ''Raw.TypePattern
mkConvertFromFreeFoil ''Raw.Type ''Raw.Ident ''Raw.ScopedType ''Raw.TypePattern

-- ** Scope-safe type patterns

mkFoilPattern ''Raw.Ident ''Raw.TypePattern
deriveCoSinkable ''Raw.Ident ''Raw.TypePattern
deriveGenericK ''FoilTypePattern
instance Foil.SinkableK FoilTypePattern
mkToFoilPattern ''Raw.Ident ''Raw.TypePattern
mkFromFoilPattern ''Raw.Ident ''Raw.TypePattern

instance Foil.UnifiablePattern FoilTypePattern where
  unifyPatterns (FoilTPatternVar x) (FoilTPatternVar y) = Foil.unifyNameBinders x y

-- * User-defined code

type Exp n = AST FoilPattern ExpSig n

type Exp' = Exp Foil.VoidS

type Type = AST FoilTypePattern TypeSig

type Type' = Type Foil.VoidS

-- ** Conversion helpers (expressions)

-- | Convert 'Raw.Exp' into a scope-safe expression.
-- This is a special case of 'unsafeConvertToAST'.
toExp :: (Foil.Distinct n) => Foil.Scope n -> Map Raw.Ident (Foil.Name n) -> Raw.Exp -> AST FoilPattern ExpSig n
toExp = unsafeConvertToAST convertToExpSig toFoilPattern getExpFromScopedExp

-- | Convert 'Raw.Exp' into a closed scope-safe expression.
-- This is a special case of 'toExp'.
toExpClosed :: Raw.Exp -> Exp Foil.VoidS
toExpClosed = toExp Foil.emptyScope Map.empty

-- | Like 'toExpClosed', but reports an unbound variable instead of failing
-- with an exception.
--
-- >>> either id show (Raw.pExp (Raw.myLexer "let x = x in x") >>= toExpClosedChecked)
-- "unbound variable: x"
-- >>> either id show (Raw.pExp (Raw.myLexer "let x = 1 in x") >>= toExpClosedChecked)
-- "let x0 = 1 in x0"
toExpClosedChecked :: Raw.Exp -> Either String (Exp Foil.VoidS)
toExpClosedChecked e =
  case checkClosed convertToExpSig patternVars getExpFromScopedExp e of
    Left (Raw.Ident x) -> Left ("unbound variable: " ++ x)
    Right () -> Right (toExpClosed e)
  where
    patternVars (Raw.PatternVar x) = [x]

-- | Convert a scope-safe representation back into 'Raw.Exp'.
-- This is a special case of 'convertFromAST'.
--
-- 'Raw.Ident' names are generated based on the raw identifiers in the underlying foil representation.
--
-- This function does not recover location information for variables, patterns, or scoped terms.
fromExp :: Exp n -> Raw.Exp
fromExp =
  convertFromAST
    convertFromExpSig
    Raw.EVar
    (fromFoilPattern mkIdent)
    Raw.ScopedExp
    (\n -> Raw.Ident ("x" ++ show n))
  where
    mkIdent n = Raw.Ident ("x" ++ show n)

-- | Parse scope-safe terms via raw representation.
--
-- >>> fromString "let x = 2 + 2 in let y = x - 1 in let x = 3 in y + x + y" :: Exp Foil.VoidS
-- let x0 = 2 + 2 in let x1 = x0 - 1 in let x2 = 3 in x1 + x2 + x1
instance IsString (Exp Foil.VoidS) where
  fromString input = case Raw.pExp (Raw.myLexer input) of
    Left err -> error ("could not parse expression: " <> input <> "\n  " <> err)
    Right term -> toExpClosed term

-- | Pretty-print scope-safe terms via"λ" Ident ":" Type "." Exp1 raw representation.
instance Show (Exp n) where
  show = Raw.printTree . fromExp

-- ** Conversion helpers (types)

-- | Convert 'Raw.Exp' into a scope-safe expression.
-- This is a special case of 'unsafeConvertToAST'.
toType :: (Foil.Distinct n) => Foil.Scope n -> Map Raw.Ident (Foil.Name n) -> Raw.Type -> AST FoilTypePattern TypeSig n
toType = unsafeConvertToAST convertToTypeSig toFoilTypePattern getTypeFromScopedType

-- | Convert 'Raw.Type' into a closed scope-safe expression.
-- This is a special case of 'toType'.
toTypeClosed :: Raw.Type -> Type Foil.VoidS
toTypeClosed = toType Foil.emptyScope Map.empty

-- | Convert a scope-safe representation back into 'Raw.Type'.
-- This is a special case of 'convertFromAST'.
--
-- 'Raw.Ident' names are generated based on the raw identifiers in the underlying foil representation.
--
-- This function does not recover location information for variables, patterns, or scoped terms.
fromType :: Type n -> Raw.Type
fromType =
  convertFromAST
    convertFromTypeSig
    Raw.TVar
    (fromFoilTypePattern mkIdent)
    Raw.ScopedType
    (\n -> Raw.Ident ("x" ++ show n))
  where
    mkIdent n = Raw.Ident ("x" ++ show n)

-- | Parse scope-safe terms via raw representation.
--
-- >>> fromString "forall x. x -> ?u1 -> Bool -> Nat" :: Type Foil.VoidS
-- forall x0 . x0 -> ?u1 -> Bool -> Nat
instance IsString (Type Foil.VoidS) where
  fromString input = case Raw.pType (Raw.myLexer input) of
    Left err -> error ("could not parse expression: " <> input <> "\n  " <> err)
    Right term -> toTypeClosed term

-- | Pretty-print scope-safe terms via"λ" Ident ":" Type "." Type1 raw representation.
instance Show (Type n) where
  show = Raw.printTree . fromType

instance Eq (Type Foil.VoidS) where
  (==) = alphaEquiv Foil.emptyScope

pattern TArrow' :: forall {binder :: Foil.S -> Foil.S -> K.Type} {q :: K.Type -> K.Type -> K.Type} {n :: Foil.S}. AST binder (Sum TypeSig q) n
                  -> AST binder (Sum TypeSig q) n -> AST binder (Sum TypeSig q) n
pattern TArrow' a b = Node (L2 (TArrowSig a b))