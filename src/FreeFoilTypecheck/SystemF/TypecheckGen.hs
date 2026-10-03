{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module FreeFoilTypecheck.SystemF.TypecheckGen where

import Control.Monad (unless)
import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Foil.Relative as Foil
import qualified Control.Monad.Free.Foil as FreeFoil
import Data.Bifoldable (Bifoldable)
import Data.Bifunctor (Bifunctor)
import Data.Bitraversable
import Data.Kind (Type)
import FreeFoilTypecheck.SystemF.FreeFoilExt

--------------------------------------------------------------------------------

-- * Core Types

--------------------------------------------------------------------------------

-- | Typing context mapping names to types
type Context' ty n = Foil.NameMap n (ty n)

-- | Scoped value with a binder
data Scoped binder (t :: Foil.S -> Type) (n :: Foil.S) where
  Scoped :: binder n l -> t l -> Scoped binder t n

-- | Type errors
data TypeError ty
  = TypeErrorUnexpectedType ty ty
  | TypeErrorUnexpectedDependentType
  deriving (Show)

-- | Bidirectional type checking result
data CheckInfer term ty (n :: Foil.S) = CheckInfer
  { check :: ty n -> Either String (),
    infer :: Either String (ty n),
    getTerm :: term n
  }

-- | Type for scoped checking/inference
type ScopedCheckInfer term binder ty (n :: Foil.S) =
  Maybe (ty n) -> CheckInfer (Scoped binder term) (Scoped binder ty) n

-- | Typed name binders data structure
data TypedNameBinders ty n l where
  TypedNameBindersEmpty :: TypedNameBinders ty n n
  TypedNameBindersCons ::
    Foil.NameBinder n i -> ty n -> TypedNameBinders ty i l -> TypedNameBinders ty n l

--------------------------------------------------------------------------------

-- * Required Type Classes

--------------------------------------------------------------------------------

-- | Alpha equivalence class
class AlphaEquiv t where
  alphaEquiv :: (Foil.Distinct n) => Foil.Scope n -> t n -> t n -> Bool

-- | Default instance for Free Foil ASTs
instance
  (Bifunctor sig, Bifoldable sig, FreeFoil.ZipMatch sig, Foil.UnifiablePattern binder) =>
  AlphaEquiv (FreeFoil.AST binder sig)
  where
  alphaEquiv = FreeFoil.alphaEquiv

-- | Main typing signature class
class
  (forall n. Show (TypeError (ty n)), AlphaEquiv ty) =>
  TypingSig binder ty sig
  where
  checkSig ::
    (Foil.Distinct n) =>
    Context' ty n ->
    sig (ScopedCheckInfer (FreeFoil.AST binder sig) binder ty n) (CheckInfer (FreeFoil.AST binder sig) ty n) ->
    ty n ->
    Either String ()
  checkSig = defaultCheckSig

  inferSig ::
    (Foil.Distinct n) =>
    Context' ty n ->
    sig (ScopedCheckInfer (FreeFoil.AST binder sig) binder ty n) (CheckInfer (FreeFoil.AST binder sig) ty n) ->
    Either String (ty n)

-- | Class for typed patterns
class TypedPattern ty pat where
  extractPatternType :: pat n l -> Maybe (ty n)
  extractTypedBinders :: pat n l -> ty n -> TypedNameBinders ty n l

-- | Class for creating trivially scoped values
class HasTrivialBinder binder where
  triviallyScoped ::
    (Foil.Distinct n, Foil.Sinkable ty) =>
    Foil.Scope n ->
    ty n ->
    Scoped binder ty n

instance HasTrivialBinder Foil.NameBinder where
  triviallyScoped scope type_ =
    Foil.withFresh scope $ \binder ->
      Scoped binder (Foil.sink type_)

--------------------------------------------------------------------------------

-- * Generic Utilities

--------------------------------------------------------------------------------

-- | Check if actual type matches expected type
shouldBe ::
  (AlphaEquiv ty, Foil.Distinct n, Show (ty n)) =>
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
            "when typechecking expression"
            -- ,  "  " ++ show e
          ]
  where
    sameType = alphaEquiv (nameMapToScope scope) actualType expectedType

-- | Default implementation of checkSig
defaultCheckSig ::
  (Foil.Distinct n, TypingSig binder ty sig) =>
  Context' ty n ->
  sig (ScopedCheckInfer (FreeFoil.AST binder sig) binder ty n) (CheckInfer (FreeFoil.AST binder sig) ty n) ->
  ty n ->
  Either String ()
defaultCheckSig ctx node expectedType = do
  inferredType <- inferSig ctx node
  unless (alphaEquiv (nameMapToScope ctx) inferredType expectedType) $
    Left (show (TypeErrorUnexpectedType inferredType expectedType))

-- | Extract type from a binder
extractTypeFromBinder ::
  (TypedPattern ty binder, AlphaEquiv ty, Foil.Distinct n) =>
  Context' ty n ->
  binder n l ->
  Maybe (ty n) ->
  Either String (ty n)
extractTypeFromBinder _scope binder Nothing =
  maybe (Left "cannot infer without type annotation for pattern") Right $
    extractPatternType binder
extractTypeFromBinder scope binder (Just ty) =
  maybe
    (Right ty)
    ( \binderTy ->
        if alphaEquiv (nameMapToScope scope) binderTy ty
          then Right ty
          else Left "type mismatch"
    )
    $ extractPatternType binder

--------------------------------------------------------------------------------

-- * Main Bidirectional Type Checking API

--------------------------------------------------------------------------------

-- | Check a term against an expected type
bidirectionalCheck ::
  ( Foil.Distinct n,
    Bitraversable sig,
    AlphaEquiv ty,
    TypingSig binder ty sig,
    Foil.UnifiablePattern binder,
    Foil.Sinkable ty,
    TypedPattern ty binder
  ) =>
  Context' ty n ->
  FreeFoil.AST binder sig n ->
  ty n ->
  Either String ()
bidirectionalCheck scope t expectedType = do
  ci <- bidirectionalCheckInfer scope t
  check ci expectedType

-- | Infer the type of a term
bidirectionalInfer ::
  ( Foil.Distinct n,
    Bitraversable sig,
    AlphaEquiv ty,
    TypingSig binder ty sig,
    Foil.UnifiablePattern binder,
    Foil.Sinkable ty,
    TypedPattern ty binder
  ) =>
  Context' ty n ->
  FreeFoil.AST binder sig n ->
  Either String (ty n)
bidirectionalInfer scope t = do
  ci <- bidirectionalCheckInfer scope t
  infer ci

-- | Combined check/infer for a term
bidirectionalCheckInfer ::
  forall ty binder sig n.
  ( Foil.Distinct n,
    Bitraversable sig,
    TypingSig binder ty sig,
    Foil.UnifiablePattern binder,
    Foil.Sinkable ty,
    TypedPattern ty binder
  ) =>
  Context' ty n ->
  FreeFoil.AST binder sig n ->
  Either String (CheckInfer (FreeFoil.AST binder sig) ty n)
-- Variable case
bidirectionalCheckInfer scope t@(FreeFoil.Var n) = do
  let inferredType = Foil.lookupName n scope
  return
    CheckInfer
      { infer = return inferredType,
        check = \expectedType -> do
          unless (alphaEquiv (nameMapToScope scope) inferredType expectedType) $
            Left (show (TypeErrorUnexpectedType inferredType expectedType)),
        getTerm = t
      }

-- Node case
bidirectionalCheckInfer scope (FreeFoil.Node node) = do
  node' <-
    bitraverse
      (bidirectionalCheckInferScoped scope)
      (bidirectionalCheckInfer scope)
      node
  return
    CheckInfer
      { infer = inferSig scope node',
        check = checkSig scope node',
        getTerm = FreeFoil.Node node
      }

extractExactlyOneBinder :: TypedPattern ty pat => pat n l -> ty n -> Foil.NameBinder n l
extractExactlyOneBinder binder ty = 
  case extractTypedBinders binder ty of
    TypedNameBindersCons extractedBinder _ty TypedNameBindersEmpty -> extractedBinder
    _ -> error "Expected exactly one binder"

-- | Bidirectional check/infer for scoped terms
bidirectionalCheckInferScoped ::
  ( Foil.Distinct n,
    Bitraversable sig,
    TypingSig binder ty sig,
    Foil.UnifiablePattern binder,
    Foil.Sinkable ty,
    TypedPattern ty binder
  ) =>
  Context' ty n ->
  FreeFoil.ScopedAST binder sig n ->
  Either String (ScopedCheckInfer (FreeFoil.AST binder sig) binder ty n)
bidirectionalCheckInferScoped scope (FreeFoil.ScopedAST binder body) =
  case (Foil.assertExt binder, Foil.assertDistinct binder) of
    (Foil.Ext, Foil.Distinct) -> return $ \mbinderType ->
      CheckInfer
        { infer = do
            ty <- extractTypeFromBinder scope binder mbinderType
            let scope' = Foil.sink <$> Foil.addNameBinder (extractExactlyOneBinder binder ty) ty scope
            ci <- bidirectionalCheckInfer scope' body
            Scoped binder <$> infer ci,
          check = \(Scoped binder' expectedType) -> do
            -- TODO: check binder' against binder
            ty <- extractTypeFromBinder scope binder mbinderType
            case Foil.unifyPatterns binder binder' of
              Foil.SameNameBinders _binders -> do
                let scope' =
                      Foil.sink <$> Foil.addNameBinder (extractExactlyOneBinder binder ty) ty scope
                ci <- bidirectionalCheckInfer scope' body
                check ci expectedType
              Foil.RenameLeftNameBinder _binders renameL ->
                case (Foil.assertExt binder', Foil.assertDistinct binder') of
                  (Foil.Ext, Foil.Distinct) -> do
                    let scope' =
                          Foil.sink <$> Foil.addNameBinder (extractExactlyOneBinder binder' ty) ty scope
                        body' =
                          Foil.liftRM
                            (nameMapToScope scope')
                            (Foil.fromNameBinderRenaming renameL)
                            body
                    ci <- bidirectionalCheckInfer scope' body'
                    check ci expectedType

              -- FIXME: RenameRightNameBinder, RenameBothNameBinders
              _ -> Left "non-unifiable patterns",
          getTerm = Scoped binder body
        }