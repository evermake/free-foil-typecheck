-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language Parser.

module HM.Parser.Abs where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

import qualified Data.Data    as C (Data, Typeable)
import qualified GHC.Generics as C (Generic)

data Pattern = PatternVar Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Term
    = EVar Ident
    | ETrue
    | EFalse
    | ENat Integer
    | EAdd Term Term
    | ESub Term Term
    | EIf Term Term Term
    | EIsZero Term
    | ETyped Term Term
    | ELet Pattern Term ScopedTerm
    | EAbsTyped Pattern Term ScopedTerm
    | EAbsUntyped Pattern ScopedTerm
    | EApp Term Term
    | ETAbs Pattern ScopedTerm
    | ETApp Term Term
    | EFor Pattern Term Term ScopedTerm
    | TUVar UVarIdent
    | TNat
    | TBool
    | TArrow Term Term
    | TForAll Pattern ScopedTerm
    | TType
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data ScopedTerm = ScopedTerm Term
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic, Data.String.IsString)

newtype UVarIdent = UVarIdent String
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic, Data.String.IsString)

