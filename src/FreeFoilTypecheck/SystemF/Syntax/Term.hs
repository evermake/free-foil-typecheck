{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module FreeFoilTypecheck.SystemF.Syntax.Term where

import qualified Control.Monad.Foil as Foil
import Control.Monad.Free.Foil
import Control.Monad.Free.Foil.TH
import Data.Bifunctor.TH
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (IsString (..))
import qualified FreeFoilTypecheck.SystemF.Parser.Abs as Raw
import qualified FreeFoilTypecheck.SystemF.Parser.Par as Raw
import qualified FreeFoilTypecheck.SystemF.Parser.Print as Raw
import FreeFoilTypecheck.SystemF.Syntax.Pattern hiding (getPatternBinder)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XDataKinds
-- >>> import qualified Control.Monad.Foil as Foil
-- >>> import Control.Monad.Free.Foil
-- >>> import Data.String (fromString)

-- * Generated code (terms)

-- ** Signature

mkSignature ''Raw.Term ''Raw.Ident ''Raw.ScopedTerm ''Raw.Pattern
deriveZipMatch ''TermSig
deriveBifunctor ''TermSig
deriveBifoldable ''TermSig
deriveBitraversable ''TermSig

-- ** Pattern synonyms

mkPatternSynonyms ''TermSig
{-# COMPLETE Var, ETrue, EFalse, ENat, EAdd, ESub, EIf, EIsZero, ETyped, ELet, EAbsTyped, EAbsUntyped, EApp, ETApp, ETAbs, EFor, TUVar, TNat, TBool, TArrow, TForAll #-}

-- ** Conversion helpers

mkConvertToFreeFoil ''Raw.Term ''Raw.Ident ''Raw.ScopedTerm ''Raw.Pattern
mkConvertFromFreeFoil ''Raw.Term ''Raw.Ident ''Raw.ScopedTerm ''Raw.Pattern

-- * User-defined code

type Term n = AST FoilPattern TermSig n
type Term' = Term Foil.VoidS

-- ** Conversion helpers (terms)

-- | Convert 'Raw.Term' into a scope-safe term.
-- This is a special case of 'convertToAST'.
toTerm :: (Foil.Distinct n) => Foil.Scope n -> Map Raw.Ident (Foil.Name n) -> Raw.Term -> AST FoilPattern TermSig n
toTerm = convertToAST convertToTermSig toFoilPattern getTermFromScopedTerm

-- | Convert 'Raw.Term' into a closed scope-safe term.
-- This is a special case of 'toTerm'.
toTermClosed :: Raw.Term -> Term Foil.VoidS
toTermClosed = toTerm Foil.emptyScope Map.empty

-- | Convert a scope-safe representation back into 'Raw.Term'.
-- This is a special case of 'convertFromAST'.
--
-- 'Raw.VarIdent' names are generated based on the raw identifiers in the underlying foil representation.
--
-- This function does not recover location information for variables, patterns, or scoped terms.
fromTerm :: Term n -> Raw.Term
fromTerm =
  convertFromAST
    convertFromTermSig
    -- (\_ -> error "location missing")
    (Raw.EVar)
    (fromFoilPattern mkVarIdent)
    Raw.ScopedTerm
    mkVarIdent
  where
    mkVarIdent n = Raw.Ident ("x" ++ show n)


-- | Parse scope-safe terms via raw representation.
--
-- >>> fromString "let x = 2 + 2 in let y = x - 1 in let x = 3 in y + x + y" :: Term Foil.VoidS
-- let x0 = 2 + 2 in let x1 = x0 - 1 in let x2 = 3 in x1 + x2 + x1
instance IsString (Term Foil.VoidS) where
  fromString input = case Raw.pTerm (Raw.myLexer input) of
    Left err -> error ("could not parse term: " <> input <> "\n  " <> err)
    Right term -> toTermClosed term

-- | Pretty-print scope-safe terms via"λ" Ident ":" Type "." Term1 raw representation.
instance Show (Term n) where
  show = Raw.printTree . fromTerm

-- | Determine if given Term is a type or not 
-- isType :: Term n -> Bool
-- isType (TUVar _) = True 
-- isType (TNat) = True 
-- isType (TType) = True 
-- isType (TBool) = True 
-- isType (TArrow _ _) = True 
-- isType (TForAll _ _) = True 
-- isType _ = False
