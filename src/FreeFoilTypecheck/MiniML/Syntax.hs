{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Scope-safe syntax of MiniML, generated from @grammar/miniml.cf@ with
-- free-foil's 'mkFreeFoil' (configured in "FreeFoilTypecheck.MiniML.FreeFoilConfig").
--
-- A pattern ('Pattern') binds its variables from left to right, and the body
-- of a binder is in the scope of all of them. The branches of @case@
-- ('Branch') are a subterm syntax, each with its own pattern.
module FreeFoilTypecheck.MiniML.Syntax where

import qualified Control.Monad.Foil as Foil
import Control.Monad.Free.Foil.TH.MkFreeFoil (mkFreeFoil, mkFreeFoilConversions)
import Data.Bifoldable (bifoldMap)
import Data.Bifunctor.TH
import Data.List (group, sort)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.String (IsString (..))
import Data.ZipMatchK (ZipMatchK (..), zipMatchViaEq)
import Data.ZipMatchK.TH (deriveZipMatchK)
import FreeFoilTypecheck.MiniML.FreeFoilConfig
import qualified FreeFoilTypecheck.MiniML.Parser.Abs as Raw
import qualified FreeFoilTypecheck.MiniML.Parser.Par as Raw
import qualified FreeFoilTypecheck.MiniML.Parser.Print as Raw
import FreeFoilTypecheck.ScopeCheck (checkClosed)
import Generics.Kind.TH (deriveGenericK)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XDataKinds
-- >>> import qualified FreeFoilTypecheck.MiniML.Parser.Par as Raw
-- >>> import Data.String (fromString)

-- * Generated code (expressions)

mkFreeFoil expConfig

deriveBifunctor ''BranchSig
deriveBifunctor ''ExpSig
deriveBifoldable ''BranchSig
deriveBifoldable ''ExpSig
deriveBitraversable ''BranchSig
deriveBitraversable ''ExpSig

deriveGenericK ''Pattern
instance Foil.SinkableK Pattern

mkFreeFoilConversions expConfig

-- * Generated code (types)

mkFreeFoil typeConfig

deriveBifunctor ''TypeSig
deriveBifoldable ''TypeSig
deriveBitraversable ''TypeSig

-- | Matching two types compares the names of unification variables.
instance ZipMatchK Raw.UVarIdent where
  zipMatchWithK = zipMatchViaEq

deriveZipMatchK ''TypeSig

deriveGenericK ''TypePattern
instance Foil.SinkableK TypePattern

instance Foil.UnifiablePattern TypePattern where
  unifyPatterns (TPatternVar x) (TPatternVar y) = Foil.unifyNameBinders x y

mkFreeFoilConversions typeConfig

-- * User-defined code

type Exp' = Exp Foil.VoidS

type Type' = Type Foil.VoidS

-- ** Expressions

toExpClosed :: Raw.Exp -> Exp'
toExpClosed = toExp Foil.emptyScope Map.empty

-- | Like 'toExpClosed', but reports an unbound variable, or a variable bound
-- twice by one pattern (which Mini-ML forbids too), instead of failing with
-- an exception or letting the second binding shadow the first.
--
-- >>> either id (const "closed") (Raw.pExp (Raw.myLexer "λx. y") >>= toExpClosedChecked)
-- "unbound variable: y"
-- >>> either id (const "closed") (Raw.pExp (Raw.myLexer "λl. case l of { [] -> 0 | x :: xs -> x }") >>= toExpClosedChecked)
-- "closed"
-- >>> either id (const "closed") (Raw.pExp (Raw.myLexer "λp. case p of { (x, inl x) -> x | _ -> 0 }") >>= toExpClosedChecked)
-- "variable bound twice in a pattern: x"
toExpClosedChecked :: Raw.Exp -> Either String Exp'
toExpClosedChecked e =
  case checkClosed toExpSig patternVars expOfScopedExp e of
    Left (Raw.Ident x) -> Left ("unbound variable: " ++ x)
    Right ()
      | Just (Raw.Ident x) <- repeatedVar -> Left ("variable bound twice in a pattern: " ++ x)
      | otherwise -> Right (toExpClosed e)
  where
    repeatedVar = listToMaybe [x | p <- patternsOf e, x : _ : _ <- group (sort (patternVars p))]

-- | Variables bound by a raw pattern, from left to right.
patternVars :: Raw.Pattern -> [Raw.Ident]
patternVars = \case
  Raw.PatternWildcard -> []
  Raw.PatternVar x -> [x]
  Raw.PatternNil -> []
  Raw.PatternPair p q -> patternVars p ++ patternVars q
  Raw.PatternInl p -> patternVars p
  Raw.PatternInr p -> patternVars p
  Raw.PatternCons p q -> patternVars p ++ patternVars q

-- | Patterns of a raw term and of all its subterms.
patternsOf :: Raw.Exp -> [Raw.Pattern]
patternsOf e = case toExpSig e of
  Left _ -> []
  Right node -> bifoldMap (\(p, scoped) -> p : patternsOf (expOfScopedExp scoped)) patternsOf node

instance IsString Exp' where
  fromString input = case Raw.pExp (Raw.myLexer input) of
    Left err -> error ("could not parse expression: " <> input <> "\n  " <> err)
    Right term -> toExpClosed term

-- | Bound variables are printed as @x0@, @x1@ and so on. A term is printed on
-- one line: BNFC's printer puts the braces of @case@ on lines of their own,
-- and this instance joins the lines.
--
-- >>> "λp. case p of { (x, inl y) -> x + y | (x, _) -> x }" :: Exp'
-- λ x0 . case x0 of { (x1, inl x2) -> x1 + x2 | (x1, _) -> x1 }
-- >>> "letrec map = λf. λl. case l of { [] -> [] | x :: xs -> f x :: map f xs } in map" :: Exp'
-- letrec x0 = λ x1 . λ x2 . case x2 of { [] -> [] | x3 :: x4 -> x1 x3 :: x0 x1 x4 } in x0
-- >>> "λs. case s of { inl l -> case l of { [] -> 0 | _ -> 1 } | inr n -> n }" :: Exp'
-- λ x0 . case x0 of { inl x1 -> case x1 of { [] -> 0 | _ -> 1 } | inr x1 -> x1 }
instance Show (Exp n) where
  show = unwords . words . Raw.printTree . fromExp

-- ** Types

toTypeClosed :: Raw.Type -> Type'
toTypeClosed = toType Foil.emptyScope Map.empty

-- |
-- >>> fromString "forall a. (a -> ?b) * List a + Nat" :: Type'
-- forall x0 . (x0 -> ?b) * List x0 + Nat
instance IsString Type' where
  fromString input = case Raw.pType (Raw.myLexer input) of
    Left err -> error ("could not parse type: " <> input <> "\n  " <> err)
    Right type_ -> toTypeClosed type_

instance Show (Type n) where
  show = Raw.printTree . fromType
