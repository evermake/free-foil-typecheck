{-# LANGUAGE TemplateHaskell #-}

-- | Configuration of free-foil's 'mkFreeFoil' for the expressions and the
-- types of MiniML, used in "FreeFoilTypecheck.MiniML.Syntax". It follows the
-- configuration of the SOAS example of free-foil
-- (@haskell/soas/src/Language/SOAS/FreeFoilConfig.hs@). Template Haskell
-- requires it to be in a separate module.
module FreeFoilTypecheck.MiniML.FreeFoilConfig where

import Control.Monad.Free.Foil.TH.MkFreeFoil
import qualified FreeFoilTypecheck.MiniML.Parser.Abs as Raw

-- | Names of bound variables when converting back to raw syntax.
intToIdent :: Int -> Raw.Ident
intToIdent i = Raw.Ident ("x" ++ show i)

-- Conversions named in the configurations below. The generated code applies
-- them as functions, so they cannot be constructors.

varExp :: Raw.Ident -> Raw.Exp
varExp = Raw.EVar

scopedExp :: Raw.Exp -> Raw.ScopedExp
scopedExp = Raw.ScopedExp

expOfScopedExp :: Raw.ScopedExp -> Raw.Exp
expOfScopedExp (Raw.ScopedExp e) = e

varType :: Raw.Ident -> Raw.Type
varType = Raw.TVar

scopedType :: Raw.Type -> Raw.ScopedType
scopedType = Raw.ScopedType

typeOfScopedType :: Raw.ScopedType -> Raw.Type
typeOfScopedType (Raw.ScopedType t) = t

-- | Expressions, with patterns as binders and the branches of @case@ as a
-- subterm syntax. Annotations stay raw types.
expConfig :: FreeFoilConfig
expConfig =
  config
    FreeFoilTermConfig
      { rawIdentName = ''Raw.Ident,
        rawTermName = ''Raw.Exp,
        rawBindingName = ''Raw.Pattern,
        rawScopeName = ''Raw.ScopedExp,
        rawVarConName = 'Raw.EVar,
        rawSubTermNames = [''Raw.Branch],
        rawSubScopeNames = [],
        intToRawIdentName = 'intToIdent,
        rawVarIdentToTermName = 'varExp,
        rawTermToScopeName = 'scopedExp,
        rawScopeToTermName = 'expOfScopedExp
      }

-- | Types, with one type variable as the binder of @forall@.
typeConfig :: FreeFoilConfig
typeConfig =
  config
    FreeFoilTermConfig
      { rawIdentName = ''Raw.Ident,
        rawTermName = ''Raw.Type,
        rawBindingName = ''Raw.TypePattern,
        rawScopeName = ''Raw.ScopedType,
        rawVarConName = 'Raw.TVar,
        rawSubTermNames = [],
        rawSubScopeNames = [],
        intToRawIdentName = 'intToIdent,
        rawVarIdentToTermName = 'varType,
        rawTermToScopeName = 'scopedType,
        rawScopeToTermName = 'typeOfScopedType
      }

config :: FreeFoilTermConfig -> FreeFoilConfig
config termConfig =
  FreeFoilConfig
    { rawQuantifiedNames = [],
      freeFoilTermConfigs = [termConfig],
      freeFoilNameModifier = id,
      freeFoilScopeNameModifier = ("Scoped" ++),
      signatureNameModifier = (++ "Sig"),
      freeFoilConNameModifier = id,
      freeFoilConvertToName = ("to" ++),
      freeFoilConvertFromName = ("from" ++)
    }
