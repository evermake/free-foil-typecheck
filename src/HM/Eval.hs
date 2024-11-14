{-# LANGUAGE LambdaCase #-}

module HM.Eval where

import Control.Monad (forM)
import Control.Monad.Foil
  ( Distinct,
    addSubst,
    identitySubst,
  )
-- import qualified Control.Monad.Foil          as Foil
import Control.Monad.Free.Foil (AST (Var), substitute)
import HM.Typecheck (Context, nameMapToScope)
import HM.Syntax

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Monad.Foil (emptyScope)

-- >>> eval emptyScope "if (iszero (2 - (1 + 1))) then true else 0"
-- Right "true"
-- >>> eval emptyScope "if (iszero (2 - (true + 1))) then true else 0"
-- Left "Unsupported expression in addition"
eval :: (Distinct n) => Context n -> Term n -> Either String (Term n)
eval _scope (Var x) = Right (Var x)
eval _scope ETrue = Right ETrue
eval _scope EFalse = Right EFalse
eval _scope (ENat n) = Right (ENat n)
eval scope (EAdd l r) = do
  l' <- eval scope l
  r' <- eval scope r
  case (l', r') of
    (ENat x, ENat y) -> Right (ENat (x + y))
    _ -> Left "Unsupported expression in addition"
eval scope (ESub l r) = do
  l' <- eval scope l
  r' <- eval scope r
  case (l', r') of
    (ENat x, ENat y) -> Right (ENat (x - y))
    _ -> Left "Unsupported expression in subtraction"
eval scope (EIf cond then_ else_) = do
  cond' <- eval scope cond
  case cond' of
    ETrue -> eval scope then_
    EFalse -> eval scope else_
    _ -> Left "Unsupported condition in if statement"
eval scope (EIsZero n) =
  eval scope n >>= \case
    ENat n'
      | n' == 0 -> Right ETrue
      | otherwise -> Right EFalse
    _ -> Left "Unsupported expression in iszero"
eval scope (ETyped e _) = eval scope e
eval scope (ELet e1 (FoilPatternVar xp) e2) = do
  e1' <- eval scope e1
  let subst = addSubst identitySubst xp e1'
  eval scope (substitute (nameMapToScope scope) subst e2)
eval _scope (EAbs type_ x e) = Right (EAbs type_ x e)
eval scope (EApp e1 e2) = do
  e1' <- eval scope e1
  e2' <- eval scope e2
  case e1' of
    EAbs _ (FoilPatternVar xp) e -> do
      let subst = addSubst identitySubst xp e2'
      eval scope (substitute (nameMapToScope scope) subst e)
    _ -> Left "Unsupported expression in application"
eval scope (EFor e1 e2 (FoilPatternVar xp) expr) = do
  e1_val <- eval scope e1
  e2_val <- eval scope e2
  case (e1_val, e2_val) of
    (ENat from, ENat to) -> do
      let ys = [from .. to]
      results <- forM ys $ \y -> do
        let subst = addSubst identitySubst xp (ENat y)
        eval scope (substitute (nameMapToScope scope) subst expr)
      return (last results)
    _ -> Left "Invalid expression in the range of for-loop"
eval scope (ETApp e t) = do 
  e' <- eval scope e 
  t' <- eval scope t
  case e' of 
    (ETAbs (FoilPatternVar xp) body) -> do 
      let subst = addSubst identitySubst xp t'
      eval scope (substitute (nameMapToScope scope) subst body)
    _other -> Left ("Unexpected type application to " <> show _other)
eval _scope (ETAbs pat e) = Right (ETAbs pat e)

eval _ (TNat) = Right TNat 
eval _ (TType) = Right TType
eval _ (TBool) = Right TBool
eval _ (TArrow l r) = Right (TArrow l r)
eval _ (TForAll p b) = Right (TForAll p b)
eval _ (TUVar n) = Right (TUVar n)
