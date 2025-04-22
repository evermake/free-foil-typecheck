{-# LANGUAGE LambdaCase #-}

module FreeFoilTypecheck.SystemF.Eval where

import           Control.Monad           (forM)
import           Control.Monad.Foil      (Distinct, addSubst, identitySubst)
-- import qualified Control.Monad.Foil          as Foil
import           Control.Monad.Free.Foil (AST (Var), substitute)
import           FreeFoilTypecheck.SystemF.Syntax
import           FreeFoilTypecheck.SystemF.Typecheck            (Context, nameMapToScope)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Monad.Foil (emptyNameMap)

eval :: (Distinct n) => Context n -> AST (FoilPattern Term) TermSig n -> Either String (AST (FoilPattern Term) TermSig n)
eval _scope (Var x) = Right (Var x)
eval _scope ETrue = Right ETrue
eval _scope EFalse = Right EFalse
eval _scope (ENat n) = Right (ENat n)
eval scope (EAdd l r) = do
  l' <- eval scope l
  r' <- eval scope r
  case (l', r') of
    (ENat x, ENat y) -> Right (ENat (x + y))
    _                -> Left "Unsupported expression in addition"
eval scope (ESub l r) = do
  l' <- eval scope l
  r' <- eval scope r
  case (l', r') of
    (ENat x, ENat y) -> Right (ENat (x - y))
    _                -> Left "Unsupported expression in subtraction"
eval scope (EIf cond then_ else_) = do
  cond' <- eval scope cond
  case cond' of
    ETrue  -> eval scope then_
    EFalse -> eval scope else_
    _      -> Left "Unsupported condition in if statement"
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
eval scope (ELet e1 (FoilPatternAsc x _t) e2) = do -- TODO: consider to replace pm for (FoilPatternAsc x _t) with pat
  e1' <- eval scope e1
  let subst = addSubst identitySubst x e1'
  eval scope (substitute (nameMapToScope scope) subst e2)
eval _scope (EAbs x e) = Right (EAbs x e)
eval scope (EApp e1 e2) = do
  e1' <- eval scope e1
  e2' <- eval scope e2
  case e1' of
    EAbs (FoilPatternVar xp) e -> do
      let subst = addSubst identitySubst xp e2'
      eval scope (substitute (nameMapToScope scope) subst e)
    EAbs (FoilPatternAsc x _t) e -> do
      let subst = addSubst identitySubst x e2'
      eval scope (substitute (nameMapToScope scope) subst e)
    e -> Left ("Unsupported expression in application" <> show (Term e))
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
eval scope (EFor e1 e2 (FoilPatternAsc x _t) expr) = do -- TODO: consider to replace pm for (FoilPatternAsc x _t) with pat
  e1_val <- eval scope e1
  e2_val <- eval scope e2
  case (e1_val, e2_val) of
    (ENat from, ENat to) -> do
      let ys = [from .. to]
      results <- forM ys $ \y -> do
        let subst = addSubst identitySubst x (ENat y)
        eval scope (substitute (nameMapToScope scope) subst expr)
      return (last results)
    _ -> Left "Invalid expression in the range of for-loop"
eval scope (ETApp e t) = do
  e' <- eval scope e
  t' <- eval scope t
  case e' of
    ETAbs (FoilPatternVar xp) body -> do
      let subst = addSubst identitySubst xp t'
      eval scope (substitute (nameMapToScope scope) subst body)
  -- _other -> Left ("Unexpected type application to " <> show _other) FIXME:  Could not deduce ‘Show (AST (FoilPattern Term) TermSig n)
    _other -> Left ("Unexpected type application")
eval _scope (ETAbs pat e) = Right (ETAbs pat e)

eval _ TNat = Right TNat
eval _ TType = Right TType
eval _ TBool = Right TBool
eval _ (TArrow l r) = Right (TArrow l r)
eval _ (TForAll p b) = Right (TForAll p b)
eval _ (TUVar n) = Right (TUVar n)

-- |
-- >>> newEval emptyNameMap "if (iszero (2 - (1 + 1))) then true else false"
-- Right true
-- >>> newEval emptyNameMap "if (iszero (2 - (true + 1))) then true else 0"
-- Left "Unsupported expression in addition"
-- >>> newEval emptyNameMap "ΛX. λx:X. x"
-- Right Λ x0 . λ x1 : x0 . x1
newEval :: Distinct n => Context n -> Term n -> Either String (Term n)
newEval scope (Term a) = case eval scope a of
  Left err -> Left err
  Right a' -> Right (Term a')
