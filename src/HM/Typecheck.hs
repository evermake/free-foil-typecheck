{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}

module HM.Typecheck where

import qualified Control.Monad.Foil          as Foil
import qualified Control.Monad.Foil.Internal as Foil
import qualified Control.Monad.Free.Foil     as FreeFoil
import           Data.Bifoldable             (Bifoldable (bifoldMap))
import qualified Data.IntMap                 as IntMap
import           Data.Maybe                  (mapMaybe)
import qualified HM.Parser.Print             as Raw
import           HM.Syntax
import           Unsafe.Coerce               (unsafeCoerce)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import HM.Parser.Abs (Type' (..))

-- | Typechecks an expression and maybe returns an error.
-- >>> typecheckClosed "2 - (1 + 1)" TNat
-- Right TNat
-- >>> typecheckClosed "2 - (1 + true)" TNat
-- Left "expected type\n  TNat\nbut got type\n  Bool\nwhen typechecking expession\n  true\n"
-- >>> typecheckClosed "2 - (1 + 1)" TBool
-- Left "expected type\n  TBool\nbut got type\n  Nat\nwhen typechecking expession\n  2 - (1 + 1)\n"
-- >>> typecheckClosed "let x = 1 in let y = 2 in x + (let x = 3 in x + y)" TNat
-- Right TNat
typecheckClosed
  :: Term Foil.VoidS {- exp -}
  -> Term Foil.VoidS {- type -}
  -> Either String (Term Foil.VoidS) {- type -}
typecheckClosed = typecheck Foil.emptyNameMap

type Context n = Foil.NameMap n (Maybe (Term n))

extendContext :: Foil.Distinct n => Foil.NameBinder n l -> Maybe (Term n) -> Context n -> Context l
extendContext binder type_ =
  case (Foil.assertExt binder, Foil.assertDistinct binder) of
    (Foil.Ext, Foil.Distinct) ->
      fmap (fmap Foil.sink) . Foil.addNameBinder binder type_

typecheck
  :: Foil.Distinct n
  => Context n
  -> Term n {- exp -}
  -> Term n {- type -}
  -> Either String (Term n) {- type -} -- TODO: convert to (Maybe (Term n))
typecheck scope e expectedType = do
  typeOfE <- inferType scope e
  -- if typeOfE == expectedType
  -- if FreeFoil.alphaEquiv typeOfE expectedType -- TODO: figure out alphaEquiv
  --   then return typeOfE
  --   else
  Left $
    unlines
      [ "expected type",
        "  " ++ show expectedType,
        "but got type",
        "  " ++ Raw.printTree (fromTerm typeOfE),
        "when typechecking expession",
        "  " ++ show e
      ]

inferType
  :: (Foil.Distinct n)
  => Context n
  -> Term n
  -> Either String (Term n) -- TODO: convert to (Maybe (Term n))
inferType scope (FreeFoil.Var n) = -- Γ, x : T ⊢ x : T
  case (Foil.lookupName n scope) of 
    Just t -> Right t
    Nothing -> undefined -- TODO: figure out what to do here
inferType _scope ETrue = return TBool
inferType _scope EFalse = return TBool
inferType _scope (ENat _) = return TNat
inferType scope (EAdd l r) = do
  _ <- typecheck scope l TNat
  _ <- typecheck scope r TNat
  return TNat
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

      let newScope = extendContext binder (Just type1) scope -- Γ' = Γ, x : type1
      type' <- inferType newScope e2 -- Γ' ⊢ e2 : ?
      unsinkType scope type'
inferType scope (EAbs type_ (FoilPatternVar x) e) = do
  case Foil.assertDistinct x of
    Foil.Distinct -> do
      -- Γ ⊢ λx : type_. e : ?
      let newScope = extendContext x (Just type_) scope -- Γ' = Γ, x : type_
      type' <- inferType newScope e
      TArrow type_ <$> unsinkType scope type'
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
      let newScope = extendContext x (Just TNat) scope
      type' <- inferType newScope expr
      unsinkType scope type'
inferType scope (ETAbs pat@(FoilPatternVar x) e) = do  
  case Foil.assertDistinct x of
    Foil.Distinct -> do
      let newScope = extendContext x Nothing scope
      type' <- inferType newScope e
      TForAll pat <$> unsinkType newScope type'
inferType scope (ETApp e t) = do 
  eType <- inferType scope e
  case eType of
    TForAll (FoilPatternVar binder) tbody -> do
      let subst = Foil.addSubst Foil.identitySubst binder t
        in return (FreeFoil.substitute (nameMapToScope scope) subst tbody)
    _ -> Left ("unexpected type application (not a forall)")
inferType scope (TForAll (FoilPatternVar tv) _expr) = do
  let _newScope = extendContext tv Nothing scope
  -- type' <- 
  error "TODO"
inferType _ _ = error "TODO"

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
