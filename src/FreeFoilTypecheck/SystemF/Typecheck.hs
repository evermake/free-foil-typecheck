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

import Control.Monad (unless)
import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Foil.Internal as Foil
import qualified Control.Monad.Foil.Relative as Foil
import qualified Control.Monad.Free.Foil as FreeFoil
import Data.Bifoldable (Bifoldable (bifoldMap))
import Data.Bifunctor (Bifunctor)
import Data.Bitraversable
import qualified Data.IntMap as IntMap
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import FreeFoilTypecheck.SystemF.Syntax
import Unsafe.Coerce (unsafeCoerce)

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
typecheckClosed ::
  Term Foil.VoidS {- exp -} ->
  Term Foil.VoidS {- type -} ->
  Either String (Term Foil.VoidS {- type -})
typecheckClosed = typecheck Foil.emptyNameMap

data Scoped binder (t :: Foil.S -> Type) (n :: Foil.S) where
  Scoped :: binder n l -> t l -> Scoped binder t n

data TypeError ty
  = TypeErrorUnexpectedType ty ty
  | TypeErrorUnexpectedDependentType
  deriving (Show)

type Context n = Foil.NameMap n (Term n)

type Context' ty n = Foil.NameMap n (ty n)

-- class BTypingSig (ty :: Foil.S -> *) (sig :: * -> * -> *) where
--   inferSigB ::
--     sig (Scoped binder ty n) (ty n) ->
--     TypeCheck n ty ty

class AlphaEquiv t where
  alphaEquiv :: (Foil.Distinct n) => Foil.Scope n -> t n -> t n -> Bool

instance
  (Bifunctor sig, Bifoldable sig, FreeFoil.ZipMatch sig, Foil.UnifiablePattern binder) =>
  AlphaEquiv (FreeFoil.AST binder sig)
  where
  alphaEquiv = FreeFoil.alphaEquiv

deriving instance AlphaEquiv Term

data CheckInfer ty (n :: Foil.S) = CheckInfer
  { check :: ty n -> Either String (),
    infer :: Either String (ty n)
  }

-- data ScopedCheckInfer pat ty (n :: Foil.S) = CheckInfer
--   { check' :: Scoped pat ty n -> Either String (),
--     infer' :: Either String (Scoped pat ty n)
--   }

type ScopedCheckInfer binder ty (n :: Foil.S) =
  Maybe (ty n) -> CheckInfer (Scoped binder ty) n

-- let x = e₁ in e₂
-- e₁ => T₁
-- x : T₁ ⊢ e₂ =>

class
  (forall n. Show (TypeError (ty n)), AlphaEquiv ty) =>
  TypingSig binder ty sig
  where
  -- to check:
  --  Γ ⊢ t₁ t₂ ⇐ B
  -- we need to
  --  infer Γ ⊢ t₁ ⇒ A → B
  --  check Γ ⊢ t₂ ⇐ A

  -- checkSig scope (EAppSig t1 t2) expectedType = do
  --   TArrow a b <- infer t1
  --   check t2 a

  --      Γ ⊢ t₁ : A
  --   Γ, x : A ⊢ t₂ : B
  -- ———————————————————————— T-Let
  -- Γ ⊢ let x : ? = t₁ in t₂ : B
  --
  -- LetSig :: term -> scope -> TermSig term scope
  --
  --   Γ, x : A ⊢ t ⇒ B
  -- ———————————————————————— T-Abs
  -- Γ ⊢ λ x : A. t ⇒ A → B
  --
  -- AbsSig₁ :: scope -> TermSig term scope
  -- AbsSig₂ :: Type -> scope -> TermSig term scope

  -- inferSig (LetSig t1 body) = do
  --   ty <- infer t1
  --   inferScoped body (Just ty)

  -- inferSig (AbsSig body) = do
  --   inferScoped body Nothing

  checkSig ::
    (Foil.Distinct n) =>
    Context' ty n -> -- context
    sig (ScopedCheckInfer binder ty n) (CheckInfer ty n) -> -- expr node
    ty n -> -- type
    Either String () -- type
  checkSig = defaultCheckSig

  inferSig ::
    (Foil.Distinct n) =>
    Context' ty n -> -- context
    sig (ScopedCheckInfer binder ty n) (CheckInfer ty n) -> -- expr node
    Either String (ty n) -- type

defaultCheckSig ::
  (Foil.Distinct n, TypingSig binder ty sig) =>
  Context' ty n -> -- context
  sig (ScopedCheckInfer binder ty n) (CheckInfer ty n) -> -- expr node
  ty n -> -- type
  Either String () -- type
defaultCheckSig ctx node expectedType = do
  inferredType <- inferSig ctx node
  unless (alphaEquiv (nameMapToScope ctx) inferredType expectedType) $
    Left (show (TypeErrorUnexpectedType inferredType expectedType))

-- data AnnotatedPattern ty pat n l = AnnotatedPattern
--   { apAnnotation :: ty n
--   , apPattern    :: pat n l
--   } deriving (CoSinkable, UnifiablePattern, ...)

-- annotateAST :: TypingSig pat ty sig => Context n -> AST pat sig n -> Either String (AST (AnnotatedPattern ty pat) sig n)
-- annotateAST scope = \case
--   Var x -> Var x
--   Node node -> Node <$> do
--     node' <- bidirectionaCheckInfer node
--     node'' <- annotateSig scope node'
--     case zipMatch node node'' of
--       Nothing -> error "fatal error"
--       Just node''' -> return $
--         bimap
--           (\(ScopedAST binder body, binderType) -> ScopedAST (AnnotatedPattern binderType binder) body)
--           (\(subterm, ()) -> subterm)
--           node'''

class HasTrivialBinder binder where
  triviallyScoped :: (Foil.Distinct n, Foil.Sinkable ty) => Foil.Scope n -> ty n -> Scoped binder ty n

instance HasTrivialBinder Foil.NameBinder where
  triviallyScoped scope type_ =
    Foil.withFresh scope $ \binder ->
      Scoped binder (Foil.sink type_)

instance HasTrivialBinder (FoilPattern Term) where
  triviallyScoped scope type_ =
    case triviallyScoped scope type_ of
      Scoped binder type' ->
        Scoped (FoilPatternVar binder) type'

instance
  (forall n. Show (TypeError (Term n))) =>
  TypingSig (FoilPattern Term) Term TermSig
  where
  -- Γ, X ⊢ λx:X.x : X → X
  -- ——————————————————————————
  -- Γ ⊢ ΛX.λx:X.x : ∀X. X → X
  checkSig scope = \case
    ETAbsSig body -> \case
      Term (TForAll x bodyType) ->
        check (body (Just (Term TType))) (Scoped x (Term bodyType))
      _ -> Left "unexpected type abstraction"
    ETAppSig e t -> \expectedType -> do
      check t (Term TType)
      (Term tt) <- infer t
      case infer e of
        Right (Term (TForAll (FoilPatternVar x) bodyType)) -> do
          let subst = Foil.addSubst Foil.identitySubst x tt
          let actualType = FreeFoil.substitute (nameMapToScope scope) subst bodyType
          (scope, Term actualType) `shouldBe` expectedType
        _ -> Left "expected a type abstraction"
    EAbsSig body -> \case
      (Term (TArrow x expectedBodyType)) -> do
        Scoped c b <- infer (body (Just (Term x)))
        case Foil.assertDistinct c of
          Foil.Distinct -> do
            b' <- unsinkType scope b
            (scope, b') `shouldBe` Term expectedBodyType
      t -> Left $ "unexpected abstraction" <> show t
    
    ELetSig e body -> \expectedType -> do
      etype <- infer e
      check
        (body (Just etype))
        (triviallyScoped (nameMapToScope scope) expectedType)

    sig -> defaultCheckSig scope sig

  -- _ -> ...
  -- ..

  inferSig scope = \case
    ETrueSig -> return (Term TBool)
    EFalseSig -> return (Term TBool)
    ENatSig _ -> return (Term TNat)
    EAddSig l r -> do
      check l (Term TNat)
      check r (Term TNat)
      return (Term TNat)
    ESubSig l r -> do
      check l (Term TNat)
      check r (Term TNat)
      return (Term TNat)
    EIfSig cond thenBranch elseBranch -> do
      check cond (Term TBool)
      thenType <- infer thenBranch
      check elseBranch thenType
      return thenType
    EIsZeroSig e -> do
      check e (Term TNat)
      return (Term TBool)
    ETypedSig e t -> do
      t' <- infer t
      check e t'
      return t'
    ELetSig e body -> do
      a <- infer e
      Scoped c bodyType <- infer (body (Just a))
      case Foil.assertDistinct c of
        Foil.Distinct ->
          unsinkType scope bodyType
    EForSig e1 e2 body -> do
      check e1 (Term TNat)
      check e2 (Term TNat)
      Scoped c bodyType <- infer (body (Just (Term TNat)))
      case Foil.assertDistinct c of
        Foil.Distinct ->
          unsinkType scope bodyType

    --  Γ ⊢ t₁ => T₁   Γ, x : T₁ ⊢ t₂ => T₂
    -- ————————————————————————————————————
    --  Γ ⊢ let x = t₁ in t₂ => T₂

    EAppSig t1 t2 ->
      infer t1 >>= \case
        Term (TArrow a b) -> do
          check t2 (Term a)
          return (Term b)
        _ -> Left "not a function"
    EAbsSig body -> do
      bodyType <- infer (body Nothing)
      case bodyType of
        Scoped (FoilPatternAsc x (Term argType)) bodyType' ->
          case Foil.assertDistinct x of
            Foil.Distinct -> do
              Term bodyType'' <- unsinkType scope bodyType'
              return (Term (TArrow argType bodyType''))
        _ -> Left "not a function" -- TODO change "not a function" to correct error
    ETAppSig body arg -> do 
      check arg (Term TType)
      (Term argType) <- infer arg
      bodyType <- infer body
      case bodyType of
        Term (TForAll (FoilPatternVar x) bodyType') -> do
          let subst = Foil.addSubst Foil.identitySubst x argType
          let actualType = FreeFoil.substitute (nameMapToScope scope) subst bodyType'
          return $ Term actualType
        _ -> Left "not a function"
    ETAbsSig body -> do
      Scoped binder typeOfBody <- infer (body (Just (Term TType)))
      case Foil.assertDistinct binder of
        Foil.Distinct -> unsinkType scope typeOfBody
    TBoolSig -> Right (Term TBool)
    TNatSig -> Right (Term TNat)
    TUVarSig n -> Right (Term (TUVar n))
    TForAllSig _a -> undefined
      -- infer (a (Just (Term TType)))
    TTypeSig -> Right (Term TType)
    TArrowSig l r -> do
      (Term lt) <- infer l
      (Term rt) <- infer r
      Right (Term (TArrow lt rt))


bidirectionalCheck ::
  (Foil.Distinct n, Bitraversable sig, AlphaEquiv ty, TypingSig binder ty sig, Foil.UnifiablePattern binder, Foil.Sinkable ty, HasExactlyOneBinder binder, TypedPattern ty binder) =>
  Context' ty n ->
  FreeFoil.AST binder sig n {- exp -} ->
  ty n {- type -} ->
  Either String ()
bidirectionalCheck scope t expectedType = do
  ci <- bidirectionaCheckInfer scope t
  check ci expectedType

bidirectionalInfer ::
  (Foil.Distinct n, Bitraversable sig, AlphaEquiv ty, TypingSig binder ty sig, Foil.UnifiablePattern binder, Foil.Sinkable ty, HasExactlyOneBinder binder, TypedPattern ty binder) =>
  Context' ty n ->
  FreeFoil.AST binder sig n {- exp -} ->
  Either String (ty n)
bidirectionalInfer scope t = do
  ci <- bidirectionaCheckInfer scope t
  infer ci

-- bidirectionalCheck scope t@(FreeFoil.Node node) expectedType =
-- node :: ??
-- TODO: typecheck node using TypingSig

bidirectionaCheckInfer ::
  (Foil.Distinct n, Bitraversable sig, TypingSig binder ty sig, Foil.UnifiablePattern binder, Foil.Sinkable ty, HasExactlyOneBinder binder, TypedPattern ty binder) =>
  Context' ty n ->
  FreeFoil.AST binder sig n {- exp -} ->
  Either String (CheckInfer ty n)
bidirectionaCheckInfer scope (FreeFoil.Var n) = do
  let inferredType = Foil.lookupName n scope
  return
    CheckInfer
      { infer = return inferredType,
        check = \expectedType -> do
          unless (alphaEquiv (nameMapToScope scope) inferredType expectedType) $
            Left (show (TypeErrorUnexpectedType inferredType expectedType))
      }
bidirectionaCheckInfer scope _t@(FreeFoil.Node node :: FreeFoil.AST binder sig n) = do
  node' <- bitraverse (bidirectionalCheckInferScoped scope) (bidirectionaCheckInfer scope) node
  return
    CheckInfer
      { infer = inferSig scope node',
        check = checkSig scope node'
      }

class HasExactlyOneBinder pat where
  extractExactlyOneBinder :: pat n l -> Foil.NameBinder n l

instance HasExactlyOneBinder Foil.NameBinder where
  extractExactlyOneBinder = id

instance HasExactlyOneBinder (FoilPattern ty) where
  extractExactlyOneBinder (FoilPatternVar x) = x
  extractExactlyOneBinder (FoilPatternAsc p _ty) = extractExactlyOneBinder p

data TypedNameBinders ty n l where
  TypedNameBindersEmpty :: TypedNameBinders ty n n
  TypedNameBindersCons :: Foil.NameBinder n i -> ty n -> TypedNameBinders ty i l -> TypedNameBinders ty n l

class TypedPattern ty pat where
  extractPatternType :: pat n l -> Maybe (ty n)
  extractTypedBinders :: pat n l -> ty n -> TypedNameBinders ty n l

instance TypedPattern ty (FoilPattern ty) where
  extractPatternType (FoilPatternVar _) = Nothing
  extractPatternType (FoilPatternAsc _ ty) = Just ty
  extractTypedBinders (FoilPatternVar binder) ty = TypedNameBindersCons binder ty TypedNameBindersEmpty
  extractTypedBinders (FoilPatternAsc binder ty) _ = TypedNameBindersCons binder ty TypedNameBindersEmpty

-- Γ, x : A ⊢ t  => B
-- —————————————————————
-- Γ  ⊢  λx:A.t  =>  A → B
bidirectionalCheckInferScoped ::
  (Foil.Distinct n, Bitraversable sig, TypingSig binder ty sig, Foil.UnifiablePattern binder, Foil.Sinkable ty, HasExactlyOneBinder binder, TypedPattern ty binder) =>
  Context' ty n ->
  FreeFoil.ScopedAST binder sig n {- exp -} ->
  Either String (ScopedCheckInfer binder ty n)
bidirectionalCheckInferScoped scope (FreeFoil.ScopedAST binder body) =
  case (Foil.assertExt binder, Foil.assertDistinct binder) of
    (Foil.Ext, Foil.Distinct) -> return $ \mbinderType ->
      CheckInfer
        { infer = do
            ty <- extractTypeFromBinder scope binder mbinderType
            let scope' = Foil.sink <$> Foil.addNameBinder (extractExactlyOneBinder binder) ty scope
            ci <- bidirectionaCheckInfer scope' body
            Scoped binder <$> infer ci,
          check = \(Scoped binder' expectedType) -> do
            -- TODO: check binder' against binder
            ty <- extractTypeFromBinder scope binder mbinderType
            case Foil.unifyPatterns binder binder' of
              Foil.SameNameBinders _binders -> do
                let scope' = Foil.sink <$> Foil.addNameBinder (extractExactlyOneBinder binder') ty scope
                ci <- bidirectionaCheckInfer scope' body
                check ci expectedType
              Foil.RenameLeftNameBinder _binders renameL ->
                case (Foil.assertExt binder', Foil.assertDistinct binder') of
                  (Foil.Ext, Foil.Distinct) -> do
                    let scope' = Foil.sink <$> Foil.addNameBinder (extractExactlyOneBinder binder') ty scope
                        body' = Foil.liftRM (nameMapToScope scope') (Foil.fromNameBinderRenaming renameL) body
                    ci <- bidirectionaCheckInfer scope' body'
                    check ci expectedType
              -- FIXME: RenameRightNameBinder, RenameBothNameBinders
              _ -> Left "non-unifiable patterns"
        }

extractTypeFromBinder ::
  (TypedPattern ty binder, AlphaEquiv ty, Foil.Distinct n) =>
  Context' ty n ->
  binder n l ->
  Maybe (ty n) ->
  Either String (ty n)
extractTypeFromBinder _scope binder Nothing = maybe (Left "cannot infer without type annotation for pattern") Right $ extractPatternType binder
extractTypeFromBinder scope binder (Just ty) = maybe (Right ty) (\binderTy -> if alphaEquiv (nameMapToScope scope) binderTy ty then Right ty else Left "type mismatch") $ extractPatternType binder

-- let scope' = Foil.addNameBinders binder _ (Foil.sink <$> scope)
-- typeOfBody <- bidirectionalInfer scope' body
-- return $ Scoped binder typeOfBody

typecheck' ::
  (Foil.Distinct n, Bitraversable sig, AlphaEquiv ty, TypingSig binder ty sig, Foil.UnifiablePattern binder, Foil.Sinkable ty, HasExactlyOneBinder binder, TypedPattern ty binder) =>
  Context' ty n ->
  FreeFoil.AST binder sig n {- exp -} ->
  ty n {- type -} ->
  Either String ()
typecheck' = bidirectionalCheck

extendContextPattern :: (Foil.Distinct n, Foil.CoSinkable pat) => pat n l -> Term n -> Context n -> Context l
extendContextPattern pattern = extendContext' (Foil.nameBinderListOf pattern)

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

shouldBe :: (AlphaEquiv ty, Foil.Distinct n, Show (ty n)) => (Foil.NameMap n (ty n), ty n) -> ty n -> Either String ()
shouldBe (scope, actualType) expectedType
  | sameType = return ()
  | otherwise =
      Left $
        unlines
          [ "expected type",
            -- "  " ++ show expectedType,
            "but got type",
            -- "  " ++ Raw.printTree actualType,
            "when typechecking expession"
            -- "  " ++ show e
          ]
  where
    sameType = alphaEquiv (nameMapToScope scope) actualType expectedType

typecheck ::
  (Foil.Distinct n) =>
  Context n ->
  Term n {- exp -} ->
  Term n {- type -} ->
  Either String (Term n {- type -})
  -- typecheck scope (EAbsUntyped binder body) (TArrow argType _resultType) =
typecheck scope (Term (EIf eCond eThen eElse)) expectedType = do
  _ <- typecheck scope (Term eCond) (Term TBool)
  _ <- typecheck scope (Term eThen) expectedType
  typecheck scope (Term eElse) expectedType
typecheck scope (Term (ELet e1 (FoilPatternVar binder) e2)) expectedType = do
  case Foil.assertDistinct binder of
    Foil.Distinct -> do
      type1 <- inferType scope (Term e1)
      let newScope = extendContext binder type1 scope
      case (Foil.assertDistinct binder, Foil.assertExt binder) of
        (Foil.Distinct, Foil.Ext) -> do
          type2 <- typecheck newScope (Term e2) (Foil.sink expectedType) -- FIXME
          unsinkType scope type2

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
typecheck scope e expectedType = do
  typeOfE <- inferType scope e
  (scope, typeOfE) `shouldBe` expectedType
  return typeOfE

-- data ScopedAST binder sig n where
--   ScopedAST :: binder n l -> AST binder sig l -> ScopedAST binder sig n

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

inferType ::
  (Foil.Distinct n) =>
  Context n ->
  Term n ->
  Either String (Term n)
inferType scope (Term (FreeFoil.Var n)) =
  -- Γ, x : T ⊢ x : T
  case Foil.lookupName n scope of
    Term TType -> Right (Foil.lookupName n scope)
    t -> Right t
inferType _scope (Term ETrue) = return (Term TBool)
inferType _scope (Term EFalse) = return (Term TBool)
inferType _scope (Term (ENat _)) = return (Term TNat)
inferType scope (Term (EAdd l r)) = do
  _ <- typecheck scope (Term l) (Term TNat)
  _ <- typecheck scope (Term r) (Term TNat)
  return (Term TNat)
inferType scope (Term (ESub l r)) = do
  _ <- typecheck scope (Term l) (Term TNat)
  _ <- typecheck scope (Term r) (Term TNat)
  return (Term TNat)
inferType scope (Term (EIf eCond eThen eElse)) = do
  _ <- typecheck scope (Term eCond) (Term TBool)
  typeOfThen <- inferType scope (Term eThen)
  _ <- typecheck scope (Term eElse) typeOfThen
  return typeOfThen
inferType scope (Term (EIsZero e)) = do
  _ <- typecheck scope (Term e) (Term TNat)
  return (Term TBool)
inferType scope (Term (ETyped expr type_)) = do
  typecheck scope (Term expr) (Term type_)
inferType scope (Term (ELet e1 pat e2)) = do
  case Foil.assertDistinct pat of
    Foil.Distinct -> do
      -- Γ ⊢ let x = e1 in e2 : ?
      type1 <- inferType scope (Term e1) -- Γ ⊢ e1 : type1
      let newScope = extendContextPattern pat type1 scope -- Γ' = Γ, x : type1
      type' <- inferType newScope (Term e2) -- Γ' ⊢ e2 : ?
      unsinkType scope type'
inferType scope (Term (EAbs (FoilPatternAsc x type_) e)) = do
  case Foil.assertDistinct x of
    Foil.Distinct -> do
      -- Γ ⊢ λx : type_. e : ?
      let newScope = extendContext x type_ scope -- Γ' = Γ, x : type_
      type' <- inferType newScope (Term e)
      fmap (Term . TArrow (convertTermToAST type_) . convertTermToAST) (unsinkType scope type')
inferType _scope (Term (EAbs (FoilPatternVar _x) _e)) = do
  Left "cannot infer lambda-abstraction without an explicit type annotation for the argument"

-- inferType _scope (EAbsUntyped _ _) = error "cannot infer λ-abstraction without explicit type annotation for the argument" -- TODO
inferType scope (Term (EApp e1 e2)) = do
  -- (Γ ⊢ e1) (Γ ⊢ e2) : ?
  type1 <- inferType scope (Term e1) -- Γ ⊢ e1 : type1
  case type1 of
    Term (TArrow type_ types) -> do
      _ <- typecheck scope (Term e2) (Term type_)
      return (Term types)
    _ -> Left ("expected type\n  TArrow\nbut got type\n  " <> show type1)
inferType scope (Term (EFor e1 e2 pat expr)) = do
  case Foil.assertDistinct pat of
    Foil.Distinct -> do
      _ <- typecheck scope (Term e1) (Term TNat)
      _ <- typecheck scope (Term e2) (Term TNat)
      let newScope = extendContextPattern pat (Term TNat) scope
      type' <- inferType newScope (Term expr)
      unsinkType scope type'
inferType scope (Term (ETAbs pat e)) = do
  case Foil.assertDistinct pat of
    Foil.Distinct -> do
      let newScope = extendContextPattern pat (Term TType) scope
      type' <- inferType newScope (Term e)
      fmap (Term . TForAll pat . convertTermToAST) (unsinkType newScope type')
inferType scope (Term (ETApp e t)) = do
  eType <- inferType scope (Term e)
  case eType of
    Term (TForAll (FoilPatternVar binder) tbody) -> do
      let subst = Foil.addSubst Foil.identitySubst binder t
       in return (Term (FreeFoil.substitute (nameMapToScope scope) subst tbody))
    _ -> Left "unexpected type application (not a forall)"
inferType _ (Term TNat) = Right (Term TNat)
inferType _ (Term TType) = Right (Term TType)
inferType _ (Term TBool) = Right (Term TBool)
inferType _ (Term (TArrow l r)) = Right (Term (TArrow l r))
inferType _ (Term (TForAll p b)) = Right (Term (TForAll p b))
inferType _ (Term (TUVar n)) = Right (Term (TUVar n))

unsinkType :: (Foil.Distinct l) => Context n -> Term l -> Either String (Term n)
unsinkType scope type_ = do
  case unsinkAST (nameMapToScope scope) (convertTermToAST type_) of
    Nothing -> Left "dependent types!"
    Just type'' -> return (Term type'')

convertTermToAST :: Term n -> FreeFoil.AST (FoilPattern Term) TermSig n
convertTermToAST (Term ast) = ast

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
