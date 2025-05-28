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

module FreeFoilTypecheck.SystemF.Typecheck where

import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Foil.Internal as Foil
import qualified Control.Monad.Foil.Relative as Foil
import qualified Control.Monad.Free.Foil as FreeFoil
import Data.Bifoldable (Bifoldable)
import Data.Bifunctor (Bifunctor)
import Debug.Trace (trace)
import FreeFoilTypecheck.SystemF.Syntax
import FreeFoilTypecheck.SystemF.FreeFoilExt

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> typecheckClosed "2 - (1 + 1)" "Nat"
-- Right Nat
-- >>> typecheckClosed "2 - (1 + true)" "Nat"
-- Left "expected type\n  Nat\nbut got type\n  Bool\nwhen typechecking expession\n  true\n"
-- >>> typecheckClosed "2 - (1 + 1)" "Bool"
-- Left "expected type\n  Bool\nbut got type\n  Nat\nwhen typechecking expession\n  2 - (1 + 1)\n"
-- >>> typecheckClosed "let x = 1 in let y = 2 in x + (let x = 3 in x + y)" "Nat"
-- Right Nat
typecheckClosed ::
  Term Foil.VoidS {- exp -} ->
  Term Foil.VoidS {- type -} ->
  Either String (Term Foil.VoidS {- type -})
typecheckClosed = typecheck Foil.emptyNameMap

--------------------------------------------------------------------------------
-- * Alpha Equivalence
--------------------------------------------------------------------------------

class AlphaEquiv t where
  alphaEquiv :: (Foil.Distinct n) => Foil.Scope n -> t n -> t n -> Bool

instance
  (Bifunctor sig, Bifoldable sig, FreeFoil.ZipMatch sig, Foil.UnifiablePattern binder) =>
  AlphaEquiv (FreeFoil.AST binder sig)
  where
  alphaEquiv = FreeFoil.alphaEquiv

deriving instance AlphaEquiv Term

--------------------------------------------------------------------------------
-- * Context Management
--------------------------------------------------------------------------------

type Context n = Foil.NameMap n (Term n)

extendContext :: (Foil.Distinct n) => Foil.NameBinder n l -> Term n -> Context n -> Context l
extendContext binder type_ =
  case (Foil.assertExt binder, Foil.assertDistinct binder) of
    (Foil.Ext, Foil.Distinct) ->
      fmap Foil.sink . Foil.addNameBinder binder type_

extendContext' :: (Foil.Distinct n) => Foil.NameBinderList n l -> Term n -> Context n -> Context l
extendContext' Foil.NameBinderListEmpty _type = id
extendContext' (Foil.NameBinderListCons binder binders) type_ =
  case (Foil.assertExt binder, Foil.assertDistinct binder) of
    (Foil.Ext, Foil.Distinct) ->
      extendContext' binders (Foil.sink type_) . extendContext binder type_

extendContextPattern :: (Foil.Distinct n, Foil.CoSinkable pat) => pat n l -> Term n -> Context n -> Context l
extendContextPattern pattern = extendContext' (Foil.nameBinderListOf pattern)

--------------------------------------------------------------------------------
-- * Type Checking Utilities
--------------------------------------------------------------------------------

-- | Check if actual type matches expected type
shouldBe :: (AlphaEquiv ty, Foil.Distinct n, Show (ty n)) => 
  (Foil.NameMap n (ty n), ty n) -> 
  ty n -> 
  Either String ()
shouldBe (scope, actualType) expectedType
  | sameType = return ()
  | otherwise =
      Left $
        unlines
          [ "expected type",
            "  " ++ show expectedType,
            "but got type",
            "  " ++ show actualType,
            "when typechecking expession"
            -- "  " ++ show e
          ]
  where
    sameType = alphaEquiv (nameMapToScope scope) actualType expectedType

-- | Unsink a type from a larger scope to a smaller one
unsinkType :: (Foil.Distinct l) => Context n -> Term l -> Either String (Term n)
unsinkType scope type_ = do
  case unsinkAST (nameMapToScope scope) (convertTermToAST type_) of
    Nothing -> Left "dependent types!"
    Just type'' -> return (Term type'')

-- | Convert Term to AST
convertTermToAST :: Term n -> FreeFoil.AST (FoilPattern Term) TermSig n
convertTermToAST (Term ast) = ast

--------------------------------------------------------------------------------
-- * Type Checking
--------------------------------------------------------------------------------

typecheck ::
  (Foil.Distinct n) =>
  Context n ->
  Term n {- exp -} ->
  Term n {- type -} ->
  Either String (Term n {- type -})

-- Conditionals
typecheck scope (Term (EIf eCond eThen eElse)) expectedType = do
  _ <- typecheck scope (Term eCond) (Term TBool)
  _ <- typecheck scope (Term eThen) expectedType
  typecheck scope (Term eElse) expectedType

-- Let bindings
typecheck scope (Term (ELet e1 (FoilPatternVar binder) e2)) expectedType = do
  case Foil.assertDistinct binder of
    Foil.Distinct -> do
      type1 <- inferType scope (Term e1)
      let newScope = extendContext binder type1 scope
      case (Foil.assertDistinct binder, Foil.assertExt binder) of
        (Foil.Distinct, Foil.Ext) -> do
          type2 <- typecheck newScope (Term e2) (Foil.sink expectedType)
          unsinkType scope type2

-- Lambda abstractions
-- Γ, x : A ⊢ t ⇐ B
-- ————————————————————————
-- Γ  ⊢  λx:A. t  ⇐  A → B
typecheck scope (Term (EAbs (FoilPatternAsc pat argTypeActual) body)) expectedType = do
  case expectedType of
    Term (TArrow argType resultType) -> do
      (scope, argTypeActual) `shouldBe` Term argType
      let newScope = extendContext pat (Term argType) scope
      case (Foil.assertDistinct pat, Foil.assertExt pat) of
        (Foil.Distinct, Foil.Ext) -> do
          type' <- typecheck newScope (Term body) (Foil.sink (Term resultType))
          unsinkType scope type'
    _ -> Left ("unexpected λ-abstraction when typechecking against non-functional type: " <> show expectedType)

typecheck scope (Term (EAbs (FoilPatternVar pat) body)) expectedType = do
  case expectedType of
    Term (TArrow argType resultType) -> do
      let newScope = extendContext pat (Term argType) scope
      case (Foil.assertDistinct pat, Foil.assertExt pat) of
        (Foil.Distinct, Foil.Ext) -> do
          type' <- typecheck newScope (Term body) (Foil.sink (Term resultType))
          unsinkType scope type'
    _ -> Left ("unexpected λ-abstraction when typechecking against non-functional type: " <> show expectedType)

-- Function application
-- Γ ⊢ t₁ ⇒ A → C     Γ ⊢ t₂ ⇐ A     B = C
-- ————————————————————————————————————————
--         Γ ⊢ t₁ t₂ ⇐ B
typecheck scope (Term (EApp e1 e2)) expectedType = do
  type1 <- inferType scope (Term e1)
  case type1 of
    Term (TArrow argType resultType) -> do
      (scope, Term resultType) `shouldBe` expectedType
      _ <- typecheck scope (Term e2) (Term argType)
      return expectedType
    _ -> Left ("unexpected application when typechecking against non-functional type: " <> show type1)

-- Type abstraction
--  Γ, X ⊢ t ⇐ T
-- ———————————————
-- Γ ⊢ ΛX.t ⇐ ∀X.T
typecheck scope (Term (ETAbs pat body)) expectedType = do
  case expectedType of
    Term (TForAll tpat bodyType) -> do
      case unifyScopes (nameMapToScope scope) (FreeFoil.ScopedAST pat body) (FreeFoil.ScopedAST tpat bodyType) of
        Nothing -> Left "non-unifiable patterns"
        Just (PairOfScopedAST binders body' bodyType') -> do
          let newScope = extendContext' (Foil.nameBindersList binders) (Term TType) scope
          case (Foil.assertDistinct binders, Foil.assertExt binders) of
            (Foil.Distinct, Foil.Ext) -> do
              type' <- typecheck newScope (Term body') (Term bodyType')
              unsinkType scope type'
    _ -> Left ("unexpected type abstraction when typechecking against non-forall type: " <> show expectedType)

-- Default case: infer and check
typecheck scope e expectedType = do
  typeOfE <- inferType scope e
  (scope, typeOfE) `shouldBe` expectedType
  return typeOfE

--------------------------------------------------------------------------------
-- * Type Inference
--------------------------------------------------------------------------------

inferType ::
  (Foil.Distinct n) =>
  Context n ->
  Term n ->
  Either String (Term n)

-- Variables
inferType scope (Term (FreeFoil.Var n)) =
  case Foil.lookupName n scope of
    Term TType -> Right (Foil.lookupName n scope)
    t -> Right t

-- Literals
inferType _scope (Term ETrue) = return (Term TBool)
inferType _scope (Term EFalse) = return (Term TBool)
inferType _scope (Term (ENat _)) = return (Term TNat)

-- Arithmetic operations
inferType scope (Term (EAdd l r)) = do
  _ <- typecheck scope (Term l) (Term TNat)
  _ <- typecheck scope (Term r) (Term TNat)
  return (Term TNat)
inferType scope (Term (ESub l r)) = do
  _ <- typecheck scope (Term l) (Term TNat)
  _ <- typecheck scope (Term r) (Term TNat)
  return (Term TNat)
  
-- Conditionals
inferType scope (Term (EIf eCond eThen eElse)) = do
  _ <- typecheck scope (Term eCond) (Term TBool)
  typeOfThen <- inferType scope (Term eThen)
  _ <- typecheck scope (Term eElse) typeOfThen
  return typeOfThen

-- Other operations
inferType scope (Term (EIsZero e)) = do
  _ <- typecheck scope (Term e) (Term TNat)
  return (Term TBool)

inferType scope (Term (ETyped expr type_)) = do
  typecheck scope (Term expr) (Term type_)

-- Let bindings
inferType scope (Term (ELet e1 pat e2)) = do
  case Foil.assertDistinct pat of
    Foil.Distinct -> do
      type1 <- inferType scope (Term e1) 
      let newScope = extendContextPattern pat type1 scope 
      type' <- inferType newScope (Term e2)
      unsinkType scope type'

-- Lambda abstractions
inferType scope t@(Term (EAbs (FoilPatternAsc x type_) e)) = do
  _ <- trace ("infer typed lambda " <> show t) (return ())
  let a = case Foil.assertDistinct x of
        Foil.Distinct -> do
          let newScope = extendContext x type_ scope
          type' <- inferType newScope (Term e)
          fmap (Term . TArrow (convertTermToAST type_) . convertTermToAST) (unsinkType scope type')
  trace ("infer typed lambda result " <> show a) a

inferType _scope t@(Term (EAbs (FoilPatternVar _x) _e)) = do
  _ <- trace ("infer untyped lambda " <> show t) (return ())
  Left "cannot infer lambda-abstraction without an explicit type annotation for the argument"

-- Function application
inferType scope (Term (EApp e1 e2)) = do
  type1 <- inferType scope (Term e1)
  case type1 of
    Term (TArrow type_ types) -> do
      _ <- typecheck scope (Term e2) (Term type_)
      return (Term types)
    _ -> Left ("expected type\n  TArrow\nbut got type\n  " <> show type1)

-- For loops
inferType scope (Term (EFor e1 e2 pat expr)) = do
  case Foil.assertDistinct pat of
    Foil.Distinct -> do
      _ <- typecheck scope (Term e1) (Term TNat)
      _ <- typecheck scope (Term e2) (Term TNat)
      let newScope = extendContextPattern pat (Term TNat) scope
      type' <- inferType newScope (Term expr)
      unsinkType scope type'

-- Type abstraction
inferType scope (Term (ETAbs pat e)) = do
  case Foil.assertDistinct pat of
    Foil.Distinct -> do
      let newScope = extendContextPattern pat (Term TType) scope
      type' <- inferType newScope (Term e)
      fmap (Term . TForAll pat . convertTermToAST) (unsinkType newScope type')

-- Type application
inferType scope (Term (ETApp e t)) = do
  eType <- inferType scope (Term e)
  case eType of
    Term (TForAll (FoilPatternVar binder) tbody) -> do
      let subst = Foil.addSubst Foil.identitySubst binder t
       in return (Term (FreeFoil.substitute (nameMapToScope scope) subst tbody))
    _ -> Left "unexpected type application (not a forall)"

-- Type constructors
inferType _ (Term TNat) = Right (Term TNat)
inferType _ (Term TType) = Right (Term TType)
inferType _ (Term TBool) = Right (Term TBool)
inferType _ (Term (TArrow l r)) = Right (Term (TArrow l r))
inferType _ (Term (TForAll p b)) = Right (Term (TForAll p b))
inferType _ (Term (TUVar n)) = Right (Term (TUVar n))

--------------------------------------------------------------------------------
-- * Scope Unification
--------------------------------------------------------------------------------

data PairOfScopedAST binder sig n where
  PairOfScopedAST :: Foil.NameBinders n l -> FreeFoil.AST binder sig l -> FreeFoil.AST binder sig l -> PairOfScopedAST binder sig n

unifyScopes ::
  (Foil.Distinct n, Foil.UnifiablePattern binder, Bifunctor sig) =>
  Foil.Scope n ->
  FreeFoil.ScopedAST binder sig n ->
  FreeFoil.ScopedAST binder sig n ->
  Maybe (PairOfScopedAST binder sig n)
unifyScopes scope (FreeFoil.ScopedAST binderL bodyL) (FreeFoil.ScopedAST binderR bodyR) =
  case Foil.unifyPatterns binderL binderR of
    Foil.SameNameBinders binders -> do
      Just (PairOfScopedAST binders bodyL bodyR)
    Foil.RenameLeftNameBinder binders renameL -> do
      case (Foil.assertDistinct binders, Foil.assertExt binders) of
        (Foil.Distinct, Foil.Ext) -> do
          let newScope = Foil.extendScopePattern binders scope
              bodyL' = Foil.liftRM newScope (Foil.fromNameBinderRenaming renameL) bodyL
          Just (PairOfScopedAST binders bodyL' bodyR)
    Foil.RenameRightNameBinder binders renameR -> do
      case (Foil.assertDistinct binders, Foil.assertExt binders) of
        (Foil.Distinct, Foil.Ext) -> do
          let newScope = Foil.extendScopePattern binders scope
              bodyR' = Foil.liftRM newScope (Foil.fromNameBinderRenaming renameR) bodyR
          Just (PairOfScopedAST binders bodyL bodyR')
    Foil.RenameBothBinders binders renameL renameR -> do
      case (Foil.assertDistinct binders, Foil.assertExt binders) of
        (Foil.Distinct, Foil.Ext) -> do
          let newScope = Foil.extendScopePattern binders scope
              bodyL' = Foil.liftRM newScope (Foil.fromNameBinderRenaming renameL) bodyL
              bodyR' = Foil.liftRM newScope (Foil.fromNameBinderRenaming renameR) bodyR
          Just (PairOfScopedAST binders bodyL' bodyR')
    Foil.NotUnifiable -> Nothing