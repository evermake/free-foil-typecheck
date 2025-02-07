{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}

module FreeFoilTypecheck.SystemF.Typecheck where

import Data.Bifunctor (Bifunctor)
import qualified Control.Monad.Foil          as Foil
import qualified Control.Monad.Foil.Relative as Foil
import qualified Control.Monad.Foil.Internal as Foil
import qualified Control.Monad.Free.Foil     as FreeFoil
import           Data.Bifoldable             (Bifoldable (bifoldMap))
import qualified Data.IntMap                 as IntMap
import           Data.Maybe                  (mapMaybe)
import qualified FreeFoilTypecheck.SystemF.Parser.Print             as Raw
import           FreeFoilTypecheck.SystemF.Syntax
import           Unsafe.Coerce               (unsafeCoerce)

-- $setup
-- >>> :set -XOverloadedStrings

-- | Typechecks an expression and maybe returns an error.
-- >>> typecheckClosed "2 - (1 + 1)" "Nat"
-- Right Nat
-- >>> typecheckClosed "2 - (1 + true)" "Nat"
-- Left "expected type\n  Nat\nbut got type\n  Bool\nwhen typechecking expession\n  true\n"
-- >>> typecheckClosed "2 - (1 + 1)" "Bool"
-- Left "expected type\n  Bool\nbut got type\n  Nat\nwhen typechecking expession\n  2 - (1 + 1)\n"
-- >>> typecheckClosed "let x = 1 in let y = 2 in x + (let x = 3 in x + y)" "Nat"
-- Right Nat
typecheckClosed
  :: Term Foil.VoidS {- exp -}
  -> Term Foil.VoidS {- type -}
  -> Either String (Term Foil.VoidS) {- type -}
typecheckClosed = typecheck Foil.emptyNameMap

data Scoped binder t n where
  Scoped :: binder n l -> t l -> Scoped binder t n

data TypeError ty
  = TypeErrorUnexpectedType ty ty
  | TypeErrorUnexpectedDependentType

type Context n = Foil.NameMap n (Term n)

type Context' ty n = Foil.NameMap n (ty n)

-- class BTypingSig (ty :: Foil.S -> *) (sig :: * -> * -> *) where 
--   inferSigB ::
--     sig (Scoped binder ty n) (ty n) ->
--     TypeCheck n ty ty

class AlphaEquiv t where
  alphaEquiv :: (Foil.Distinct n) => Foil.Scope n -> t n -> t n -> Bool

instance
  (Bifunctor sig, Bifoldable sig, FreeFoil.ZipMatch sig) =>
  AlphaEquiv (FreeFoil.AST binder sig)
  where
  alphaEquiv = FreeFoil.alphaEquiv

class (AlphaEquiv ty) => TypingSig binder ty sig where
  checkSig ::
    (Foil.Distinct n) =>
    Context' ty n -> -- context
    sig (Scoped binder ty n) (ty n) -> -- expr node
    ty n -> -- type
    Either (TypeError (ty n)) () -- type
  checkSig ctx node expectedType = do
    inferredType <- inferSig ctx node
    unless (alphaEquiv (nameMapToScope ctx) inferredType expectedType) $
      Left (TypeErrorUnexpectedType inferredType expectedType)
  inferSig ::
    (Foil.Distinct n) =>
    Context' ty n -> -- context
    sig (Scoped binder ty n) (ty n) -> -- expr node
    Either (TypeError (ty n)) (ty n) -- type


extendContext :: Foil.Distinct n => Foil.NameBinder n l -> Term n -> Context n -> Context l
extendContext binder type_ =
  case (Foil.assertExt binder, Foil.assertDistinct binder) of
    (Foil.Ext, Foil.Distinct) ->
      fmap Foil.sink . Foil.addNameBinder binder type_

extendContext' :: Foil.Distinct n => Foil.NameBinderList n l -> Term n -> Context n -> Context l
extendContext' Foil.NameBinderListEmpty _type = id
extendContext' (Foil.NameBinderListCons binder binders) type_ = 
  case (Foil.assertExt binder, Foil.assertDistinct binder) of
    (Foil.Ext, Foil.Distinct) ->
      extendContext' binders (Foil.sink type_) . extendContext binder type_
  
shouldBe :: Foil.Distinct n => (Foil.NameMap n (Term n), Term n) -> Term n -> Either String ()
shouldBe (scope, actualType) expectedType
  | sameType = return ()
  | otherwise = Left $
      unlines
        [ "expected type",
          "  " ++ show expectedType,
          "but got type",
          "  " ++ Raw.printTree (fromTerm actualType),
          "when typechecking expession",
          "  " -- ++ show e
        ]
  where
    sameType = FreeFoil.alphaEquiv (nameMapToScope scope) actualType expectedType 

typecheck
  :: Foil.Distinct n
  => Context n
  -> Term n {- exp -}
  -> Term n {- type -}
  -> Either String (Term n) {- type -}
-- typecheck scope (EAbsUntyped binder body) (TArrow argType _resultType) =
typecheck scope (EIf eCond eThen eElse) expectedType = do 
  _ <- typecheck scope eCond TBool 
  _ <- typecheck scope eThen expectedType 
  typecheck scope eElse expectedType
typecheck scope (ELet e1 (FoilPatternVar binder) e2) expectedType = do 
  case Foil.assertDistinct binder of
    Foil.Distinct -> do
      type1 <- inferType scope e1 
      let newScope = extendContext binder type1 scope 
      case (Foil.assertDistinct binder, Foil.assertExt binder) of
        (Foil.Distinct, Foil.Ext) -> do
          type2 <- typecheck newScope e2 (Foil.sink expectedType) -- FIXME
          unsinkType scope type2

-- Γ, x : A ⊢ t ⇐ B
-- —————————————————————          
-- Γ  ⊢  λx. t  ⇐  A → B
typecheck scope (EAbsUntyped pat body) expectedType = do
  case expectedType of
    TArrow argType _resultType ->
      typecheck scope (EAbsTyped argType pat body) expectedType
    _ -> Left ("unexpected λ-abstraction when typechecking against functional type: " <> show expectedType)

-- Γ, x : A ⊢ t ⇐ B
-- ————————————————————————
-- Γ  ⊢  λx:A. t  ⇐  A → B
typecheck scope (EAbsTyped argTypeActual (FoilPatternVar pat) body) expectedType = do
  case expectedType of
    TArrow argType resultType -> do
        (scope, argTypeActual) `shouldBe` argType
        let newScope = extendContext pat argType scope
        case (Foil.assertDistinct pat, Foil.assertExt pat) of
          (Foil.Distinct, Foil.Ext) -> do
            type' <- typecheck newScope body (Foil.sink resultType)
            unsinkType scope type'
    _ -> Left ("unexpected λ-abstraction when typechecking against non-functional type: " <> show expectedType)

-- Γ ⊢ t₁ ⇒ A → C     Γ ⊢ t₂ ⇐ A     B = C
-- ————————————————————————————————————————
--         Γ ⊢ t₁ t₂ ⇐ B
typecheck scope (EApp e1 e2) expectedType = do
  type1 <- inferType scope e1
  case type1 of
    TArrow argType resultType -> do
      (scope, resultType) `shouldBe` expectedType
      _ <- typecheck scope e2 argType
      return expectedType
    _ -> Left ("unexpected application when typechecking against non-functional type: " <> show type1)

--  Γ, X ⊢ t ⇐ T
-- ———————————————
-- Γ ⊢ ΛX.t ⇐ ∀X.T
typecheck scope (ETAbs pat body) expectedType = do
  case expectedType of
    TForAll tpat bodyType -> do
      case unifyScopes (nameMapToScope scope) (FreeFoil.ScopedAST pat body) (FreeFoil.ScopedAST tpat bodyType) of
        Nothing -> Left "non-unifiable patterns"
        Just (PairOfScopedAST binders body' bodyType') -> do
          let newScope = extendContext' (Foil.nameBindersList binders) TType scope
          case (Foil.assertDistinct binders, Foil.assertExt binders) of
            (Foil.Distinct, Foil.Ext) -> do
              type' <- typecheck newScope body' bodyType'
              unsinkType scope type'
    _ -> Left ("unexpected type abstraction when typechecking against non-forall type: " <> show expectedType)
typecheck scope e expectedType = do
  typeOfE <- inferType scope e
  (scope, typeOfE) `shouldBe` expectedType
  return typeOfE

-- data ScopedAST binder sig n where
--   ScopedAST :: binder n l -> AST binder sig l -> ScopedAST binder sig n

data PairOfScopedAST binder sig n where
  PairOfScopedAST :: Foil.NameBinders n l -> FreeFoil.AST binder sig l -> FreeFoil.AST binder sig l -> PairOfScopedAST binder sig n

unifyScopes
  :: (Foil.Distinct n, Foil.UnifiablePattern binder, Bifunctor sig)
  => Foil.Scope n
  -> FreeFoil.ScopedAST binder sig n
  -> FreeFoil.ScopedAST binder sig n
  -> Maybe (PairOfScopedAST binder sig n)
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

inferType
  :: (Foil.Distinct n)
  => Context n
  -> Term n
  -> Either String (Term n)
inferType scope (FreeFoil.Var n) = -- Γ, x : T ⊢ x : T
  case (Foil.lookupName n scope) of
    TType -> Right (FreeFoil.Var n)
    t     -> Right t
inferType _scope ETrue = return TBool
inferType _scope EFalse = return TBool
inferType _scope (ENat _) = return TNat
inferType scope (EAdd l r) = do
  _ <- typecheck scope l TNat
  _ <- typecheck scope r TNat
  return  TNat
inferType scope (ESub l r) = do
  _ <- typecheck scope l TNat
  _ <- typecheck scope r TNat
  return TNat
inferType scope (EIf eCond eThen eElse) = do
  _ <- typecheck scope eCond TBool
  typeOfThen <- inferType scope eThen
  _ <- typecheck scope eElse typeOfThen
  return typeOfThen
inferType scope (EIsZero e) = do
  _ <- typecheck scope e TNat
  return TBool
inferType scope (ETyped expr type_) = do
  typecheck scope expr type_
inferType scope (ELet e1 (FoilPatternVar binder) e2) = do
  case Foil.assertDistinct binder of
    Foil.Distinct -> do
      -- Γ ⊢ let x = e1 in e2 : ?
      type1 <- inferType scope e1 -- Γ ⊢ e1 : type1
      let newScope = extendContext binder type1 scope -- Γ' = Γ, x : type1
      type' <- inferType newScope e2 -- Γ' ⊢ e2 : ?
      unsinkType scope type'
inferType scope (EAbsTyped type_ (FoilPatternVar x) e) = do
  case Foil.assertDistinct x of
    Foil.Distinct -> do
      -- Γ ⊢ λx : type_. e : ?
      let newScope = extendContext x type_ scope -- Γ' = Γ, x : type_
      type' <- inferType newScope e
      fmap (TArrow type_)  (unsinkType scope type')
inferType _scope (EAbsUntyped _ _) = error "cannot infer λ-abstraction without explicit type annotation for the argument" -- TODO
inferType scope (EApp e1 e2) = do
  -- (Γ ⊢ e1) (Γ ⊢ e2) : ?
  type1 <- inferType scope e1 -- Γ ⊢ e1 : type1
  case type1 of
    TArrow type_ types -> do
      _ <- typecheck scope e2 type_
      return types
    _ -> Left ("expected type\n  TArrow\nbut got type\n  " <> show type1)
inferType scope (EFor e1 e2 (FoilPatternVar x) expr) = do
  case Foil.assertDistinct x of
    Foil.Distinct -> do
      _ <- typecheck scope e1 TNat
      _ <- typecheck scope e2 TNat
      let newScope = extendContext x TNat scope
      type' <- inferType newScope expr
      unsinkType scope type'
inferType scope (ETAbs pat@(FoilPatternVar x) e) = do
  case Foil.assertDistinct x of
    Foil.Distinct -> do
      let newScope = extendContext x TType scope
      type' <- inferType newScope e
      fmap (TForAll pat) (unsinkType newScope type')
inferType scope (ETApp e t) = do
  eType <- inferType scope e
  case eType of
    TForAll (FoilPatternVar binder) tbody -> do
      let subst = Foil.addSubst Foil.identitySubst binder t
        in return (FreeFoil.substitute (nameMapToScope scope) subst tbody)
    _ -> Left ("unexpected type application (not a forall)")
inferType _ (TNat) = Right TNat
inferType _ (TType) = Right TType
inferType _ (TBool) = Right TBool
inferType _ (TArrow l r) = Right (TArrow l r)
inferType _ (TForAll p b) = Right (TForAll p b)
inferType _ (TUVar n) = Right (TUVar n)

unsinkType :: Foil.Distinct l => Context n -> Term l -> Either String (Term n)
unsinkType scope type_ = do
  case unsinkAST (nameMapToScope scope) type_ of
    Nothing     -> Left "dependent types!"
    Just type'' -> return type''

-- HELPERS

-- FIXME: should be in free-foil
deriving instance Functor (Foil.NameMap n)
deriving instance Foldable (Foil.NameMap n)
deriving instance Traversable (Foil.NameMap n)

nameMapToScope :: Foil.NameMap n a -> Foil.Scope n
nameMapToScope (Foil.NameMap m) = Foil.UnsafeScope (IntMap.keysSet m)


-- TForAll :: Pattern n l -> Term l -> Term n
--
-- let z = 1 in ΛX. λy:X. z
--
-- Γ, z : Nat ⊢ ∀X. X → Nat
-- Γ          ⊢ ∀X. X → Nat
--
-- Γ, z : Nat, X ⊢ X → Nat
-- Γ, X          ⊢ X → Nat
--
-- FIXME: should be part of free-foil
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
