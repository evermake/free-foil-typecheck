{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Typing rules of the HM language for the generic engine
-- ('FreeFoilTypecheck.GeneralTypecheck').
module FreeFoilTypecheck.HindleyMilner.Rules where

import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Free.Foil as FreeFoil
import Data.Bifunctor (bimap)
import Data.Bifunctor.Sum (Sum (..))
import FreeFoilTypecheck.GeneralTypecheck
import FreeFoilTypecheck.HindleyMilner.Syntax

-- $setup
-- >>> :set -XOverloadedStrings

instance HMTypingSig FoilTypePattern TypeSig ExpSig where
  inferSigHM = \case
    ETrueSig -> return (injectUType TBool)
    EFalseSig -> return (injectUType TBool)
    ESubSig l r -> do
      _ <- unifyHM l (injectUType TNat)
      _ <- unifyHM r (injectUType TNat)
      return (injectUType TNat)
    EAddSig l r -> do
      _ <- unifyHM l (injectUType TNat)
      _ <- unifyHM r (injectUType TNat)
      return (injectUType TNat)
    EIfSig condType thenType elseType -> do
      _ <- unifyHM condType (injectUType TBool)
      _ <- unifyHM thenType elseType
      return thenType
    EIsZeroSig argType -> do
      _ <- unifyHM argType (injectUType TNat)
      return (injectUType TBool)
    EAppSig funType argType -> do
      retType <- freshHM
      _ <- unifyHM funType (FreeFoil.Node (L2 (TArrowSig argType retType)))
      return retType
    ETypedSig ty annotation -> do
      let annotationType = injectUType' (toTypeClosed annotation)
      _ <- unifyHM ty annotationType
      return annotationType
    ENatSig _ -> do
      return (injectUType TNat)
    EForSig fromTy toTy inferBody -> do
      (binderType, bodyType) <- inferBody Nothing
      _ <- unifyHM fromTy (injectUType TNat)
      _ <- unifyHM toTy (injectUType TNat)
      _ <- unifyHM binderType (injectUType TNat)
      return bodyType
    EAbsSig inferBody -> do
      (paramType, bodyType) <- inferBody Nothing
      return (TArrow' paramType bodyType)
    ELetSig type1 inferBody -> do
      gtype1 <- generalizeHM type1
      (_, bodyType) <- inferBody (Just gtype1)
      return bodyType

instance TypedPattern (UType binder typeSig) FoilPattern where
  enterScopePattern (FoilPatternVar binder) = enterScopePattern binder

injectUType' :: FreeFoil.AST binder TypeSig n -> UType binder TypeSig n
injectUType' = transAST $ \case
  TUVarSig m -> R2 (MetaVarSig m)
  node -> L2 node

instance Show (UType FoilTypePattern TypeSig n) where
  show = show . fromUType
    where
      fromUType :: UType FoilTypePattern TypeSig n -> Type n
      fromUType = \case
        FreeFoil.Var x -> FreeFoil.Var x
        FreeFoil.Node (L2 node) -> FreeFoil.Node (bimap fromUTypeScoped fromUType node)
        FreeFoil.Node (R2 (MetaVarSig metavar)) -> FreeFoil.Node (TUVarSig metavar)

      fromUTypeScoped :: UScopedType FoilTypePattern TypeSig n -> FreeFoil.ScopedAST FoilTypePattern TypeSig n
      fromUTypeScoped (FreeFoil.ScopedAST binder body) = FreeFoil.ScopedAST binder (fromUType body)

-- | Infer the type of a closed term with the generic engine.
--
-- >>> testInferTypeNewClosed "1 + 2"
-- Right Nat
-- >>> testInferTypeNewClosed "1 + true"
-- Left "cannot unify "
testInferTypeNewClosed :: Exp Foil.VoidS -> Either String (UType FoilTypePattern TypeSig Foil.VoidS)
testInferTypeNewClosed e = fst <$> runTypeCheck (inferTypeNewClosed e) emptyTypingContext
