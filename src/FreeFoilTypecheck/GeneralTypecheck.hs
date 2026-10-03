{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-simplifiable-class-constraints #-}

module FreeFoilTypecheck.GeneralTypecheck where

import Control.Monad (ap)
import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Foil.Internal as Foil
import qualified Control.Monad.Foil.Relative as Foil
import qualified Control.Monad.Free.Foil as FreeFoil

import Control.Monad.Free.Foil.Generic (genericZipMatch2)
import qualified Control.Monad.Free.Foil.Generic as FreeFoil
import Data.Bifoldable
import Data.Bifunctor
import Data.Bifunctor.Sum
import Data.Bifunctor.TH
import Data.Bitraversable (Bitraversable (..))
import qualified Data.IntMap as IntMap
import qualified Data.Kind as K
import qualified FreeFoilTypecheck.HindleyMilner.Parser.Abs as Raw
import qualified GHC.Generics as GHC
import Generics.Kind.TH (deriveGenericK)

type Constraint' ty = (ty, ty)

type USubst_ tyn = (Raw.UVarIdent, tyn)

-- -- ∀ x₁ x₂ … xₙ. T
-- -- Type scheme (a.k.a. polytype).
data TypeScheme ty where
  TypeScheme :: Foil.NameBinderList Foil.VoidS n -> ty n -> TypeScheme ty

data HMType ty
  = MonoType (ty Foil.VoidS)
  | PolyType (TypeScheme ty)

-- -- ty -- ?
-- -- 1. automatically make ∀-types => need to manage bound variables in types
-- -- 2. automatically add unification variables:
-- --   * need to unify (zipMatch) types — easier to generate
-- --   * need to inject or use exisiting unification variables
-- -- conclusion — ty should be generated via free foil
-- --
-- -- ty n = AST binder (typeSig :+: MetaVarSig) n
-- --

newtype MetaVarSig scope term = MetaVarSig Raw.UVarIdent
  deriving (Eq, Show, Functor, GHC.Generic)

deriveGenericK ''MetaVarSig

instance FreeFoil.ZipMatchK Raw.UVarIdent where
  zipMatchWithK = FreeFoil.zipMatchViaEq

instance FreeFoil.ZipMatchK MetaVarSig

instance FreeFoil.ZipMatch MetaVarSig where
  zipMatch = genericZipMatch2

deriveBifunctor ''MetaVarSig
deriveBifoldable ''MetaVarSig
deriveBitraversable ''MetaVarSig

type UType binder typeSig = FreeFoil.AST binder (Sum typeSig MetaVarSig)

type UScopedType binder typeSig = FreeFoil.ScopedAST binder (Sum typeSig MetaVarSig)

fromUVarIdent :: Raw.UVarIdent -> UType binder typeSig n
fromUVarIdent x = FreeFoil.Node (R2 (MetaVarSig x))

toUVarIdent :: UType binder typeSig n -> Maybe Raw.UVarIdent
toUVarIdent (FreeFoil.Node (R2 (MetaVarSig x))) = Just x
toUVarIdent _ = Nothing

data TypingContext ty n = TypingContext
  { tcConstraints :: [Constraint' (ty Foil.VoidS)],
    tcSubsts :: [USubst_ (ty Foil.VoidS)],
    tcTypings :: Foil.NameMap n (HMType ty),
    tcFreshId :: Int
  }

newtype TypeCheck ty n a = TypeCheck {runTypeCheck :: TypingContext ty n -> Either String (a, TypingContext ty n)}
  deriving (Functor)

localTypingContext ::
  (TypingContext ty n -> TypingContext ty l) ->
  (TypingContext ty l -> TypingContext ty n) ->
  TypeCheck ty l a ->
  TypeCheck ty n a
localTypingContext f f' (TypeCheck g) = TypeCheck $ \ctx -> do
  (x, ctx') <- g (f ctx)
  return (x, f' ctx')

failTypeCheck :: String -> TypeCheck (UType binder typeSig) n a
failTypeCheck msg = TypeCheck (\_ctx -> Left msg)

instance Applicative (TypeCheck (UType binder typeSig) n) where
  pure x = TypeCheck $ \tc -> Right (x, tc)
  (<*>) = ap

instance Monad (TypeCheck (UType binder typeSig) n) where
  --   return x = TypeCheck $ \tc -> Right (x, tc)

  --   (>>=) :: TypeCheck a -> (a -> TypeCheck b) -> TypeCheck b
  --   g :: TypingContext n -> Either String (a, TypingContext n)
  --   TypeCheck g >>= f = TypeCheck $ \tc ->
  --     case g tc of
  --       Left err -> Left err
  --       Right (x, tc') -> runTypeCheck (f x) tc'

  --   do
  --    x <- TypeCheck g
  --    f x
  TypeCheck g >>= f = TypeCheck $ \tc -> do
    (x, tc') <- g tc
    runTypeCheck (f x) tc'

inferTypeNewClosed ::
  (Foil.CoSinkable typeBinder, Bitraversable sig, HMTypingSig typeBinder typeSig sig, Bitraversable typeSig, FreeFoil.ZipMatch typeSig, TypedPattern (UType typeBinder typeSig) binder) =>
  FreeFoil.AST binder sig Foil.VoidS ->
  TypeCheck (UType typeBinder typeSig) Foil.VoidS (UType typeBinder typeSig Foil.VoidS)
inferTypeNewClosed expr = do
  type' <- reconstructType expr
  TypingContext {tcSubsts = substs, tcConstraints = constrs} <- get
  substs' <- unify (map (applySubstsToConstraint substs) constrs)
  return (applySubstsToType substs' type')

emptyTypingContext :: TypingContext ty Foil.VoidS
emptyTypingContext = TypingContext [] [] Foil.emptyNameMap 0

type Infer typeBinder typeSig = UType typeBinder typeSig Foil.VoidS

type ScopedInfer typeBinder typeSig n =
  Maybe (HMType (UType typeBinder typeSig)) ->
  TypeCheck (UType typeBinder typeSig) n (Infer typeBinder typeSig, Infer typeBinder typeSig)

class HMTypingSig (binder :: Foil.S -> Foil.S -> K.Type) (typeSig :: K.Type -> K.Type -> K.Type) (sig :: K.Type -> K.Type -> K.Type) where
  inferSigHM ::
    sig
      (ScopedInfer binder typeSig n)
      (Infer binder typeSig) -> -- expr node
    TypeCheck (UType binder typeSig) n (Infer binder typeSig) -- typecheck result

class AlphaEquiv t where
  alphaEquiv :: (Foil.Distinct n) => Foil.Scope n -> t n -> t n -> Bool

instance
  (Bifunctor sig, Bifoldable sig, FreeFoil.ZipMatch sig, Foil.UnifiablePattern binder) =>
  AlphaEquiv (FreeFoil.AST binder sig)
  where
  alphaEquiv = FreeFoil.alphaEquiv

equivHMType :: (Foil.RelMonad Foil.Name ty) => (forall n. (Foil.Distinct n) => Foil.Scope n -> ty n -> ty n -> Bool) -> HMType ty -> HMType ty -> Bool
equivHMType alphaEquivFunc t1 t2 =
  equivTypeScheme alphaEquivFunc (toTypeScheme t1) (toTypeScheme t2)

unwrapMonoType :: HMType ty -> ty Foil.VoidS
unwrapMonoType (MonoType ty) = ty
unwrapMonoType (PolyType _) = undefined

toTypeScheme :: HMType ty -> TypeScheme ty
toTypeScheme (PolyType typeScheme) = typeScheme
toTypeScheme (MonoType ty) = TypeScheme Foil.NameBinderListEmpty ty

equivTypeScheme :: (Foil.RelMonad Foil.Name ty) => (forall n. (Foil.Distinct n) => Foil.Scope n -> ty n -> ty n -> Bool) -> TypeScheme ty -> TypeScheme ty -> Bool
equivTypeScheme alphaEquivFunc (TypeScheme binders1 ty1) (TypeScheme binders2 ty2) =
  case Foil.unifyPatterns binders1 binders2 of
    Foil.SameNameBinders {} ->
      case Foil.assertDistinct binders1 of
        Foil.Distinct ->
          let scope = Foil.extendScopePattern binders1 Foil.emptyScope
           in alphaEquivFunc scope ty1 ty2
    Foil.RenameLeftNameBinder _ rename1to2 ->
      case Foil.assertDistinct binders2 of
        Foil.Distinct ->
          let scope = Foil.extendScopePattern binders2 Foil.emptyScope
           in alphaEquivFunc scope (Foil.liftRM scope (Foil.fromNameBinderRenaming rename1to2) ty1) ty2
    Foil.RenameRightNameBinder _ rename2to1 ->
      case Foil.assertDistinct binders1 of
        Foil.Distinct ->
          let scope = Foil.extendScopePattern binders1 Foil.emptyScope
           in alphaEquivFunc scope ty1 (Foil.liftRM scope (Foil.fromNameBinderRenaming rename2to1) ty2)
    Foil.RenameBothBinders binders' rename1 rename2 ->
      case Foil.assertDistinct binders' of
        Foil.Distinct ->
          let scope = Foil.extendScopePattern binders' Foil.emptyScope
           in alphaEquivFunc
                scope
                (Foil.liftRM scope (Foil.fromNameBinderRenaming rename1) ty1)
                (Foil.liftRM scope (Foil.fromNameBinderRenaming rename2) ty2)
    Foil.NotUnifiable -> False

injectUType :: (Bifunctor typeSig) => FreeFoil.AST binder typeSig n -> UType binder typeSig n
injectUType = transAST L2

-- L2 :: typeSig scope term -> (Sum typeSig MetaVarSig) scope term

transAST ::
  (Bifunctor sig1) =>
  (forall scope term. sig1 scope term -> sig2 scope term) ->
  FreeFoil.AST binder sig1 n ->
  FreeFoil.AST binder sig2 n
transAST _ (FreeFoil.Var x) = FreeFoil.Var x
transAST phi (FreeFoil.Node node) = FreeFoil.Node (phi (bimap (transScopedAST phi) (transAST phi) node))

transScopedAST ::
  (Bifunctor sig1) =>
  (forall scope term. sig1 scope term -> sig2 scope term) ->
  FreeFoil.ScopedAST binder sig1 n ->
  FreeFoil.ScopedAST binder sig2 n
transScopedAST phi (FreeFoil.ScopedAST binder body) =
  FreeFoil.ScopedAST binder (transAST phi body)

applySubstsFromContextToType ::
  (Foil.Distinct n, Bifunctor typeSig, Foil.CoSinkable binder, Foil.DExt Foil.VoidS n) =>
  UType binder typeSig n ->
  TypeCheck (UType binder typeSig) n (UType binder typeSig n)
applySubstsFromContextToType type_ = do
  TypingContext _ substs _ _ <- get
  return (applySubstsToType (map (fmap Foil.sink) substs) type_)

generalizeHM ::
  (FreeFoil.ZipMatch typeSig, Bitraversable typeSig, Bifunctor typeSig, Bifoldable typeSig, Foil.CoSinkable binder) =>
  UType binder typeSig Foil.VoidS ->
  TypeCheck (UType binder typeSig) n (HMType (UType binder typeSig))
generalizeHM whatTyp = do
  unifyTypeCheck
  (TypingContext _ substs ctx _) <- get
  let whatTyp1 = applySubstsToType substs whatTyp
  let ctx' = fmap (applySubstsToHMType substs) ctx
  let ctxVars = foldl (\idents typ -> idents ++ allUVarsOfHMType typ) [] ctx'
  let whatFreeIdents = filter (\i -> not (elem i ctxVars)) (allUVarsOfType whatTyp1)

  let whatTyp2 = generalize whatFreeIdents whatTyp1
  return whatTyp2

unifyHM :: (FreeFoil.ZipMatch typeSig, Bitraversable typeSig, Foil.CoSinkable binder) => UType binder typeSig Foil.VoidS -> UType binder typeSig Foil.VoidS -> TypeCheck (UType binder typeSig) n ()
unifyHM typ1 typ2 = do
  -- traceShow (unsafeCoerce (typ1, typ2) :: (UType FoilTypePattern TypeSig Foil.VoidS, UType FoilTypePattern TypeSig Foil.VoidS)) $ do
  TypingContext {..} <- get
  let typ1' = applySubstsToType tcSubsts typ1
      typ2' = applySubstsToType tcSubsts typ2
  case (typ1', typ2') of
    -- Case for unification variables
    (toUVarIdent -> Just x, toUVarIdent -> Just y)
      | x == y -> return ()
    (toUVarIdent -> Just x, r) -> bindUVar x r
    (l, toUVarIdent -> Just x) -> bindUVar x l
    -- Case for Free Foil variables (not supported for now)
    (FreeFoil.Var x, FreeFoil.Var y)
      | x == y -> addSubsts []
    -- Case of non-trivial arbitrary nodes
    (FreeFoil.Node l, FreeFoil.Node r) ->
      -- zipMatch (TArrowSig x1 x2) (TArrowSig y1 y2)
      --   = Just (TArrowSig (x1, y1) (x2, y2))
      case FreeFoil.zipMatch l r of
        Nothing -> failTypeCheck ("cannot unify ") -- ++ l ++ r)
        -- `zipMatch` takes out corresponding terms from a node that we need
        --  to unify further.
        Just lr -> do
          bitraverse_ (\_ -> failTypeCheck "Unable to unify scoped type") (uncurry unifyHM) lr -- ignores "scopes", only works with "terms"
    (_, _) -> failTypeCheck ("cannot unify ") -- ++ lhs ++ rhs)
  where
    bindUVar x typ
      | x `elem` allUVarsOfType typ = failTypeCheck "occurs check failed"
      | otherwise = addSubsts [(x, typ)]

freshHM :: TypeCheck (UType binder typeSig) n (UType binder typeSig Foil.VoidS)
freshHM = do
  TypingContext constraints substs ctx freshId <- get
  put (TypingContext constraints substs ctx (freshId + 1))
  return (fromUVarIdent (makeIdent freshId))

withGeneralizedVars ::
  (Foil.Distinct n) =>
  Foil.Scope n ->
  [(a, Foil.Name n)] ->
  [a] ->
  (forall l. Foil.NameBinderList n l -> [(a, Foil.Name l)] -> r) ->
  r
withGeneralizedVars scope env xs cont =
  case xs of
    [] -> cont Foil.NameBinderListEmpty (map (fmap Foil.sink) env)
    y : ys -> Foil.withFresh scope $ \binder ->
      let scope' = Foil.extendScope binder scope
          env' = (y, Foil.nameOf binder) : map (fmap Foil.sink) env
       in withGeneralizedVars scope' env' ys $ \nameBinderList env'' ->
            cont (Foil.NameBinderListCons binder nameBinderList) env''

generalize :: (Bifunctor typeSig, Foil.CoSinkable binder) => [Raw.UVarIdent] -> UType binder typeSig Foil.VoidS -> HMType (UType binder typeSig)
generalize = go Foil.emptyScope
  where
    go _ [] type_ = MonoType type_
    go scope xs type_ = withGeneralizedVars scope [] xs $ \freshNameBinders env ->
      case (Foil.assertExt freshNameBinders, Foil.assertDistinct freshNameBinders) of
        (Foil.Ext, Foil.Distinct) ->
          let substs = map (fmap FreeFoil.Var) env
              type' = applySubstsToType substs (Foil.sink type_)
           in PolyType (TypeScheme freshNameBinders type')

(+++) :: (Foil.Distinct n, Bifunctor typeSig, Foil.CoSinkable binder) => [USubst_ (UType binder typeSig n)] -> [USubst_ (UType binder typeSig n)] -> [USubst_ (UType binder typeSig n)]
xs +++ ys = map (applySubstsInSubsts ys) xs ++ ys

unify ::
  (FreeFoil.ZipMatch typeSig, Bitraversable typeSig, Foil.CoSinkable binder) =>
  [Constraint' (UType binder typeSig Foil.VoidS)] ->
  TypeCheck (UType binder typeSig) n ([USubst_ (UType binder typeSig Foil.VoidS)])
unify constraints = do
  -- 'unifyHM' applies the current substitution and extends it in the context
  mapM_ (uncurry unifyHM) constraints
  TypingContext _ substs _ _ <- get
  return substs

unifyWith ::
  (FreeFoil.ZipMatch typeSig, Bitraversable typeSig, Foil.CoSinkable binder) =>
  [USubst_ (UType binder typeSig Foil.VoidS)] ->
  [Constraint' (UType binder typeSig Foil.VoidS)] ->
  TypeCheck (UType binder typeSig) n [USubst_ (UType binder typeSig Foil.VoidS)]
unifyWith substs constraints = unify (map (applySubstsToConstraint substs) constraints)

applySubstsToConstraint :: (Foil.Distinct n, Bifunctor typeSig, Foil.CoSinkable binder) => [USubst_ (UType binder typeSig n)] -> Constraint' (UType binder typeSig n) -> Constraint' (UType binder typeSig n)
applySubstsToConstraint substs (l, r) = (applySubstsToType substs l, applySubstsToType substs r)

applySubstToType :: (Foil.Distinct n, Bifunctor typeSig, Foil.CoSinkable binder) => USubst_ (UType binder typeSig n) -> UType binder typeSig n -> UType binder typeSig n
applySubstToType (ident, typ) type_@(toUVarIdent -> Just x)
  | ident == x = typ
  | otherwise = type_
applySubstToType _ type_@FreeFoil.Var {} = type_
applySubstToType subst (FreeFoil.Node node) =
  FreeFoil.Node (bimap (applySubstToScopedType subst) (applySubstToType subst) node)

applySubstToScopedType :: (Foil.Distinct n, Bifunctor typeSig, Foil.CoSinkable binder) => USubst_ (UType binder typeSig n) -> UScopedType binder typeSig n -> UScopedType binder typeSig n
applySubstToScopedType subst' (FreeFoil.ScopedAST binder body) =
  case (Foil.assertExt binder, Foil.assertDistinct binder) of
    (Foil.Ext, Foil.Distinct) ->
      FreeFoil.ScopedAST binder (applySubstToType (fmap Foil.sink subst') body)

applySubstsToType :: (Foil.Distinct n, Bifunctor typeSig, Foil.CoSinkable binder) => [USubst_ (UType binder typeSig n)] -> UType binder typeSig n -> UType binder typeSig n
applySubstsToType [] typ = typ
applySubstsToType (subst : rest) typ = applySubstsToType rest (applySubstToType subst typ)

applySubstsToHMType :: (Bifunctor typeSig, Foil.CoSinkable binder) => [USubst_ (UType binder typeSig Foil.VoidS)] -> HMType (UType binder typeSig) -> HMType (UType binder typeSig)
applySubstsToHMType substs (PolyType (TypeScheme freeVars ty)) =
  case (Foil.assertExt freeVars, Foil.assertDistinct freeVars) of
    (Foil.Ext, Foil.Distinct) -> PolyType (TypeScheme freeVars (applySubstsToType (map (fmap Foil.sink) substs) ty))
applySubstsToHMType substs (MonoType ty) =
  MonoType (applySubstsToType substs ty)

applySubstsInSubsts :: (Foil.Distinct n, Bifunctor typeSig, Foil.CoSinkable binder) => [USubst_ (UType binder typeSig n)] -> USubst_ (UType binder typeSig n) -> USubst_ (UType binder typeSig n)
applySubstsInSubsts substs (l, r) = (l, (applySubstsToType substs r))

deriving instance Functor (Foil.NameMap n)

deriving instance Foldable (Foil.NameMap n)

get :: TypeCheck ty n (TypingContext ty n)
get = TypeCheck $ \tc -> Right (tc, tc)

put :: TypingContext ty n -> TypeCheck ty n ()
put new = TypeCheck $ \_old -> Right ((), new)

eitherToTypeCheck :: Either String a -> TypeCheck (UType binder typeSig) n a
eitherToTypeCheck (Left err) = TypeCheck $ \_tc -> Left err
eitherToTypeCheck (Right x) = TypeCheck $ \tc -> Right (x, tc)

unifyTypeCheck :: (FreeFoil.ZipMatch typeSig, Bitraversable typeSig, Foil.CoSinkable binder) => TypeCheck (UType binder typeSig) n ()
unifyTypeCheck = do
  TypingContext constraints substs ctx freshId <- get
  put (TypingContext [] substs ctx freshId)
  _ <- unifyWith substs constraints
  return ()

class TypedPattern ty binder where
  enterScopePattern :: binder n l -> HMType ty -> TypeCheck ty l a -> TypeCheck ty n a

instance TypedPattern (UType binder typeSig) Foil.NameBinder where
  enterScopePattern = enterScope

enterScope :: Foil.NameBinder n l -> HMType (UType binder typeSig) -> TypeCheck (UType binder typeSig) l a -> TypeCheck (UType binder typeSig) n a
enterScope x type_ action =
  localTypingContext
    (\_ctx@TypingContext {..} -> TypingContext {tcTypings = Foil.addNameBinder x type_ tcTypings, ..})
    (\_ctx@TypingContext {..} -> TypingContext {tcTypings = popNameBinder x tcTypings, ..})
    action

popNameBinder :: Foil.NameBinder n l -> Foil.NameMap l a -> Foil.NameMap n a
popNameBinder name (Foil.NameMap m) = Foil.NameMap (IntMap.delete (Foil.nameId (Foil.nameOf name)) m)

addConstraints :: [Constraint' (UType binder typeSig Foil.VoidS)] -> TypeCheck (UType binder typeSig) n ()
addConstraints constrs = do
  TypingContext constraints substs ctx freshId <- get
  put (TypingContext (constrs ++ constraints) substs ctx freshId)

addSubsts :: (Bifunctor typeSig, Foil.CoSinkable binder) => [USubst_ (UType binder typeSig Foil.VoidS)] -> TypeCheck (UType binder typeSig) n ()
addSubsts substs = do
  TypingContext constraints substs' ctx freshId <- get
  let constraints' = map (applySubstsToConstraint substs) constraints
      substs'' = substs' +++ substs
      ctx' = fmap (applySubstsToHMType substs) ctx
  put (TypingContext constraints' substs'' ctx' freshId)

reconstructType ::
  (Foil.CoSinkable typeBinder, Bitraversable sig, Bifunctor typeSig, HMTypingSig typeBinder typeSig sig, TypedPattern (UType typeBinder typeSig) binder) =>
  FreeFoil.AST binder sig n ->
  TypeCheck (UType typeBinder typeSig) n (UType typeBinder typeSig Foil.VoidS)
reconstructType = \case
  FreeFoil.Var x -> lookupVarInTypingContext x
  FreeFoil.Node node -> do
    -- :: TypeCheck' ty n (sig (HMType ty, ty Foil.VoidS) (ty Foil.VoidS))
    node' <- bitraverse reconstructTypeScoped' reconstructType node
    inferSigHM node'

reconstructTypeScoped' ::
  (Foil.CoSinkable typeBinder, Bitraversable sig, Bifunctor typeSig, HMTypingSig typeBinder typeSig sig, TypedPattern (UType typeBinder typeSig) binder) =>
  FreeFoil.ScopedAST binder sig n ->
  TypeCheck (UType typeBinder typeSig) n (ScopedInfer typeBinder typeSig n)
reconstructTypeScoped' (FreeFoil.ScopedAST binder body) = do
  -- _
  return $ \case
    Nothing -> do
      type_ <- freshTypeVar
      bodyType <- enterScopePattern binder (MonoType type_) $ do
        reconstructType body
      return (type_, bodyType)
    Just gtype -> do
      bodyType <- enterScopePattern binder gtype $ do
        reconstructType body
      return (undefined, bodyType)

freshMonoType :: TypeCheck (UType typeBinder typeSig) n (HMType (UType typeBinder typeSig))
freshMonoType = do
  TypingContext _ _ _ freshId <- get
  updateFreshId (freshId + 1)
  return (MonoType (fromUVarIdent (makeIdent freshId)))

freshTypeVar :: TypeCheck (UType typeBinder typeSig) n (UType typeBinder typeSig Foil.VoidS)
freshTypeVar = do
  TypingContext _ _ _ freshId <- get
  updateFreshId (freshId + 1)
  return (fromUVarIdent (makeIdent freshId))

lookupVarInTypingContext :: (Bifunctor typeSig, Foil.CoSinkable typeBinder) => Foil.Name n -> TypeCheck (UType typeBinder typeSig) n (UType typeBinder typeSig Foil.VoidS)
lookupVarInTypingContext x = do
  TypingContext _ _ ctx freshId <- get
  let xTyp = Foil.lookupName x ctx
  let (specTyp, freshId2) = specialize xTyp freshId
  updateFreshId freshId2
  return specTyp

specializeHM :: (Bifunctor typeSig, Foil.CoSinkable typeBinder) => HMType (UType typeBinder typeSig) -> TypeCheck (UType typeBinder typeSig) n (UType typeBinder typeSig Foil.VoidS)
specializeHM x = do
  TypingContext _ _ _ freshId <- get
  let (specTyp, freshId2) = specialize x freshId
  updateFreshId freshId2
  return specTyp

updateFreshId :: Int -> TypeCheck (UType binder typeSig) n ()
updateFreshId freshId = do
  TypingContext constrs subst ctx _ <- get
  put (TypingContext constrs subst ctx freshId)

specialize :: (Bifunctor typeSig, Foil.CoSinkable typeBinder) => HMType (UType typeBinder typeSig) -> Int -> (UType typeBinder typeSig Foil.VoidS, Int)
specialize (PolyType (TypeScheme list ty)) freshId = go Foil.emptyScope list ty freshId
  where
    go :: (Foil.Distinct n, Bifunctor typeSig, Foil.CoSinkable typeBinder) => Foil.Scope n -> Foil.NameBinderList n l -> UType typeBinder typeSig l -> Int -> (UType typeBinder typeSig n, Int)
    go _ Foil.NameBinderListEmpty ty_ freshId_ = (ty_, freshId_)
    go scope (Foil.NameBinderListCons binder bs) ty_ freshId_ =
      case Foil.assertDistinct binder of
        Foil.Distinct ->
          let subst = Foil.addSubst Foil.identitySubst binder (fromUVarIdent (makeIdent freshId_))
              scope' = Foil.extendScope binder scope
              (ty', freshId') = go scope' bs ty_ (freshId_ + 1)
           in (FreeFoil.substitute scope subst ty', freshId')
specialize (MonoType ty) freshId = (ty, freshId)

-- let f = λx:X?. (let g = λy:Y?. x in g) in f
--
-- g : Y? → X?
-- g : ∀ α. α → X?                    -- generalize g
-- let g = λy:Y?. x in g : A? → X?    -- specialize g
-- f : X? → A? → X?
-- f : ∀ α β. α → β → α
-- let f = λx:X?. (let g = λy:Y?. x in g) in f  : B? → C? → B?  -- specialize f

-- ∀ α β. α → (β → G?) → A?
-- local variables: α, β
-- unification variables: G?, A?

allUVarsOfHMType :: (Bifoldable typeSig) => HMType (UType binder typeSig) -> [Raw.UVarIdent]
allUVarsOfHMType (MonoType a) = allUVarsOfType a
allUVarsOfHMType (PolyType (TypeScheme _list ty)) = allUVarsOfType ty

allUVarsOfType :: (Bifoldable typeSig) => UType binder typeSig n -> [Raw.UVarIdent]
allUVarsOfType (toUVarIdent -> Just ident) = [ident]
allUVarsOfType (FreeFoil.Var _) = []
allUVarsOfType (FreeFoil.Node node) = bifoldMap allUVarsOfScopedType allUVarsOfType node

allUVarsOfScopedType :: (Bifoldable typeSig) => UScopedType binder typeSig n -> [Raw.UVarIdent]
allUVarsOfScopedType (FreeFoil.ScopedAST _binder body) = allUVarsOfType body

makeIdent :: Int -> Raw.UVarIdent
makeIdent i = Raw.UVarIdent ("?u" ++ (show i))

-- * Orphans

deriving instance Traversable (Foil.NameMap n)

instance Foil.UnifiablePattern Foil.NameBinderList where
  unifyPatterns Foil.NameBinderListEmpty Foil.NameBinderListEmpty =
    Foil.SameNameBinders Foil.emptyNameBinders
  unifyPatterns Foil.NameBinderListEmpty (Foil.NameBinderListCons _ _) =
    Foil.NotUnifiable
  unifyPatterns (Foil.NameBinderListCons _ _) Foil.NameBinderListEmpty =
    Foil.NotUnifiable
  unifyPatterns (Foil.NameBinderListCons x xs) (Foil.NameBinderListCons y ys) =
    case (Foil.assertDistinct x, Foil.assertDistinct y) of
      (Foil.Distinct, Foil.Distinct) -> Foil.unifyNameBinders x y `Foil.andThenUnifyPatterns` (xs, ys)
