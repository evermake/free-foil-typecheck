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

module FreeFoilTypecheck.SystemF.TypingSig where

import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Free.Foil as FreeFoil
import FreeFoilTypecheck.SystemF.FreeFoilExt
import FreeFoilTypecheck.SystemF.Syntax
import FreeFoilTypecheck.SystemF.TypecheckGen

--------------------------------------------------------------------------------

-- * TypingSig Instance for System F

--------------------------------------------------------------------------------

instance
  (forall n. Show (TypeError (Term n))) =>
  TypingSig (FoilPattern Term) Term TermSig
  where
  --------------------------------------------------------------------------------
  -- Type Checking Rules
  --------------------------------------------------------------------------------

  checkSig scope = \case
    -- Type abstraction: Γ ⊢ ΛX.t ⇐ ∀X.T
    ETAbsSig body -> \case
      Term (TForAll x bodyType) ->
        check (body (Just (Term TType))) (Scoped x (Term bodyType))
      _ -> Left "unexpected type abstraction"
    -- Type application
    ETAppSig e t -> \expectedType -> do
      check t (Term TType)
      (Term tt) <- infer t
      case infer e of
        Right (Term (TForAll (FoilPatternVar x) bodyType)) -> do
          let subst = Foil.addSubst Foil.identitySubst x tt
          let actualType = FreeFoil.substitute (nameMapToScope scope) subst bodyType
          (scope, Term actualType) `shouldBe` expectedType
        _ -> Left "expected a type abstraction"

    -- Lambda abstraction: Γ ⊢ λx:A. t ⇐ A → B
    EAbsSig body -> \case
      Term (TArrow x expectedBodyType) -> do
        Scoped c b <- infer (body (Just (Term x)))
        case Foil.assertDistinct c of
          Foil.Distinct -> do
            b' <- unsinkType scope b
            (scope, b') `shouldBe` Term expectedBodyType
      t -> Left $ "unexpected abstraction" <> show t
    -- Let binding
    ELetSig e body -> \expectedType -> do
      etype <- infer e
      check
        (body (Just etype))
        (triviallyScoped (nameMapToScope scope) expectedType)

    -- Default case
    sig -> defaultCheckSig scope sig

  --------------------------------------------------------------------------------
  -- Type Inference Rules
  --------------------------------------------------------------------------------

  inferSig scope = \case
    -- Literals
    ETrueSig -> return (Term TBool)
    EFalseSig -> return (Term TBool)
    ENatSig _ -> return (Term TNat)
    -- Arithmetic operations
    EAddSig l r -> do
      check l (Term TNat)
      check r (Term TNat)
      return (Term TNat)
    ESubSig l r -> do
      check l (Term TNat)
      check r (Term TNat)
      return (Term TNat)

    -- Conditionals
    EIfSig cond thenBranch elseBranch -> do
      check cond (Term TBool)
      thenType <- infer thenBranch
      check elseBranch thenType
      return thenType

    -- Zero test
    EIsZeroSig e -> do
      check e (Term TNat)
      return (Term TBool)

    -- Type annotation
    -- t : T
    -- T : Type
    ETypedSig e t -> do
      check t (Term TType)
      let t' = Term (getTerm t)
      check e t'
      return t'

    -- Let binding: Γ ⊢ let x = t₁ in t₂ ⇒ T₂
    ELetSig e body -> do
      a <- infer e
      Scoped c bodyType <- infer (body (Just a))
      case Foil.assertDistinct c of
        Foil.Distinct ->
          unsinkType scope bodyType

    -- For loop
    EForSig e1 e2 body -> do
      check e1 (Term TNat)
      check e2 (Term TNat)
      Scoped c bodyType <- infer (body (Just (Term TNat)))
      case Foil.assertDistinct c of
        Foil.Distinct ->
          unsinkType scope bodyType

    -- Function application: Γ ⊢ t₁ t₂ ⇒ B
    EAppSig t1 t2 ->
      infer t1 >>= \case
        Term (TArrow a b) -> do
          check t2 (Term a)
          return (Term b)
        _ -> Left "not a function"
    -- Lambda abstraction (with annotation)
    EAbsSig body -> do
      bodyType <- infer (body Nothing)
      case bodyType of
        Scoped (FoilPatternAsc x (Term argType)) bodyType' ->
          case Foil.assertDistinct x of
            Foil.Distinct -> do
              Term bodyType'' <- unsinkType scope bodyType'
              return (Term (TArrow argType bodyType''))
        _ -> Left "cannot infer type of unannotated lambda"

    -- Type application
    ETAppSig body arg -> do
      check arg (Term TType)
      (Term argType) <- infer arg
      bodyType <- infer body
      case bodyType of
        Term (TForAll (FoilPatternVar x) bodyType') -> do
          let subst = Foil.addSubst Foil.identitySubst x argType
          let actualType = FreeFoil.substitute (nameMapToScope scope) subst bodyType'
          return $ Term actualType
        _ -> Left "expected a polymorphic type"

    -- Type abstraction
    -- ΛX.λx.x  :  forall X. X -> X
    -- body = λx.x : X -> X
    ETAbsSig body -> do
      Scoped binder (Term typeOfBody) <- infer (body (Just (Term TType)))
      case Foil.assertDistinct binder of
        Foil.Distinct -> return (Term (TForAll binder typeOfBody))

    -- Type constructors
    TBoolSig -> Right (Term TType)
    TNatSig -> Right (Term TType)
    TUVarSig{} -> Right (Term TType)
    TTypeSig -> Right (Term TType)
    -- Type-level forall
    TForAllSig body -> do -- forall X. T
      Scoped _binder _bodyType <- infer (body (Just (Term TType)))
      -- TODO: check that bodyType is just Type
      return (Term TType)

    -- Arrow type
    TArrowSig l r -> do
      check l (Term TType)
      check r (Term TType)
      Right (Term TType)

type Context n = Foil.NameMap n (Term n)

type Context' ty n = Foil.NameMap n (ty n)

--------------------------------------------------------------------------------

-- * Utility Functions

--------------------------------------------------------------------------------

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

-- * Typeclass Instances

--------------------------------------------------------------------------------

instance HasTrivialBinder (FoilPattern Term) where
  triviallyScoped scope type_ =
    case triviallyScoped scope type_ of
      Scoped binder type' ->
        Scoped (FoilPatternVar binder) type'

instance TypedPattern ty (FoilPattern ty) where
  extractPatternType (FoilPatternVar _) = Nothing
  extractPatternType (FoilPatternAsc _ ty) = Just ty
  extractTypedBinders (FoilPatternVar binder) ty =
    TypedNameBindersCons binder ty TypedNameBindersEmpty
  extractTypedBinders (FoilPatternAsc binder ty) _ =
    TypedNameBindersCons binder ty TypedNameBindersEmpty

deriving instance AlphaEquiv Term