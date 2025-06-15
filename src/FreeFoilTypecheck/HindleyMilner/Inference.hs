{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-simplifiable-class-constraints #-}

module FreeFoilTypecheck.HindleyMilner.Inference where

import Control.Monad (ap)
import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Foil.Internal as Foil
import qualified Control.Monad.Free.Foil as FreeFoil
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Foldable as F
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Hashable
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (trace)
import qualified FreeFoilTypecheck.HindleyMilner.Parser.Abs as Raw
import FreeFoilTypecheck.HindleyMilner.Syntax

-- $setup
-- >>> :set -XOverloadedStrings

--------------------------------------------------------------------------------

deriving instance Functor (Foil.NameMap n)

deriving instance Foldable (Foil.NameMap n)

deriving instance (Show a) => Show (Foil.NameMap n a)

popNameBinder :: Foil.NameBinder n l -> Foil.NameMap l a -> Foil.NameMap n a
popNameBinder name (Foil.NameMap m) = Foil.NameMap (IntMap.delete (Foil.nameId (Foil.nameOf name)) m)

--------------------------------------------------------------------------------

type IdentLevelMap = HashMap.HashMap Raw.UVarIdent Int

instance Data.Hashable.Hashable Raw.UVarIdent where
  hashWithSalt salt (Raw.UVarIdent s) = Data.Hashable.hashWithSalt salt s

newtype Constraint n = Constraint (Type n, Type n)

type Constraint' = Constraint Foil.VoidS

deriving instance Show (Constraint n)

newtype Subst n = Subst (Map.Map Raw.UVarIdent (Type n))

type Subst' = Subst Foil.VoidS

deriving instance Show (Subst n)

newtype TypingEnv n tn = TypingEnv (Foil.NameMap n (Type tn))

type TypingEnv' n = TypingEnv n Foil.VoidS

deriving instance Show (TypingEnv n tn)

class Typed a where
  applySubst :: (Foil.Distinct n) => Subst n -> a n -> a n

  -- | Returns a set of all free (unification) variables of a Typed.
  freeVars :: a n -> Set.Set Raw.UVarIdent

  -- | Returns whether a free (unification) variable occurs in a Typed.
  hasFreeVar :: Raw.UVarIdent -> a n -> Bool
  hasFreeVar i t = i `Set.member` freeVars t

instance Typed (FreeFoil.AST FoilTypePattern TypeSig) where
  applySubst (Subst s) t = foldr applySubstToType t (Map.toList s)

  freeVars (TUVar ident) = Set.singleton ident
  freeVars (FreeFoil.Var _) = Set.empty
  freeVars (FreeFoil.Node node) = Set.unions $ freeVars <$> node

instance Typed Constraint where
  applySubst s (Constraint (t1, t2)) = Constraint (applySubst s t1, applySubst s t2)

  freeVars (Constraint (t1, t2)) = Set.union (freeVars t1) (freeVars t2)

instance Typed (TypingEnv n) where
  applySubst s (TypingEnv env) = TypingEnv (fmap (applySubst s) env)

  freeVars (TypingEnv env) = Set.unions $ freeVars <$> env

nullSubst :: Subst Foil.VoidS
nullSubst = Subst Map.empty

singleSubst :: (Foil.Distinct n) => Raw.UVarIdent -> Type n -> Subst n
singleSubst ident type_ = Subst (Map.singleton ident type_)

composeSubst :: (Foil.Distinct n) => Subst n -> Subst n -> Subst n
composeSubst (Subst s1) (Subst s2) = Subst (Map.map (applySubst (Subst s2)) s1 `Map.union` s2)

applySubstToType :: (Foil.Distinct n) => (Raw.UVarIdent, Type n) -> Type n -> Type n
applySubstToType (ident, type_) (TUVar x)
  | ident == x = type_
  | otherwise = TUVar x
applySubstToType _ (FreeFoil.Var x) = FreeFoil.Var x
applySubstToType subst (FreeFoil.Node node) =
  FreeFoil.Node (bimap (applySubstToScopedType subst) (applySubstToType subst) node)

applySubstToScopedType :: (Foil.Distinct n) => (Raw.UVarIdent, Type n) -> FreeFoil.ScopedAST FoilTypePattern TypeSig n -> FreeFoil.ScopedAST FoilTypePattern TypeSig n
applySubstToScopedType subst (FreeFoil.ScopedAST binder body) =
  case (Foil.assertExt binder, Foil.assertDistinct binder) of
    (Foil.Ext, Foil.Distinct) ->
      FreeFoil.ScopedAST binder (applySubstToType (fmap Foil.sink subst) body)

--------------------------------------------------------------------------------

data TypingContext n = TypingContext
  { tcConstraints :: [Constraint'],
    tcSubst :: Subst',
    tcEnv :: TypingEnv' n,
    tcFreshId :: Int,
    tcLevelMap :: IdentLevelMap,
    tcLevel :: Int
  }

initialTypingContext :: TypingContext Foil.VoidS
initialTypingContext =
  TypingContext
    { tcConstraints = [],
      tcSubst = Subst Map.empty,
      tcEnv = TypingEnv Foil.emptyNameMap,
      tcFreshId = 1,
      tcLevelMap = HashMap.empty,
      tcLevel = 1
    }

--------------------------------------------------------------------------------

newtype TypeInferencer n a = TypeInferencer
  {runTypeInferencer :: TypingContext n -> Either String (a, TypingContext n)}
  deriving (Functor)

instance Applicative (TypeInferencer n) where
  pure x = TypeInferencer $ \ctx -> Right (x, ctx)
  (<*>) = ap

instance Monad (TypeInferencer n) where
  TypeInferencer g >>= f = TypeInferencer $ \ctx -> do
    (x, ctx') <- g ctx
    runTypeInferencer (f x) ctx'

evalTypeInferencer :: TypeInferencer Foil.VoidS a -> Either String a
evalTypeInferencer ti = do
  (result, _ctx) <- runTypeInferencer ti initialTypingContext
  return result

get :: TypeInferencer n (TypingContext n)
get = TypeInferencer $ \ctx -> Right (ctx, ctx)

gets :: (TypingContext n -> a) -> TypeInferencer n a
gets f = TypeInferencer $ \ctx -> Right (f ctx, ctx)

put :: TypingContext n -> TypeInferencer n ()
put newCtx = TypeInferencer $ \_old -> Right ((), newCtx)

fromEither :: Either String a -> TypeInferencer n a
fromEither (Left err) = TypeInferencer $ \_ctx -> Left err
fromEither (Right x) = TypeInferencer $ \ctx -> Right (x, ctx)

enterScope :: Foil.NameBinder n l -> Type' -> TypeInferencer l a -> TypeInferencer n a
enterScope binder type_ action = do
  ctx <- get
  let (TypingEnv nameMap) = tcEnv ctx
  let ctx' = ctx {tcEnv = TypingEnv (Foil.addNameBinder binder type_ nameMap)}
  (x, ctx'') <- fromEither $ runTypeInferencer action ctx'
  let (TypingEnv nameMap'') = tcEnv ctx''
  put ctx'' {tcEnv = TypingEnv (popNameBinder binder nameMap'')}
  return x

enterLevel :: TypeInferencer n a -> TypeInferencer n a
enterLevel action = do
  ctx <- get
  let ctx' = ctx {tcLevel = tcLevel ctx + 1}
  (x, ctx'') <- fromEither $ runTypeInferencer action ctx'
  put ctx'' {tcLevel = tcLevel ctx'' - 1}
  return x

addConstraints :: [(Type', Type')] -> TypeInferencer n ()
addConstraints constrs = do
  ctx <- get
  put ctx {tcConstraints = (Constraint <$> constrs) ++ tcConstraints ctx}

freshUVar :: TypeInferencer n (Type', Raw.UVarIdent)
freshUVar = do
  ctx <- get
  let freshId = tcFreshId ctx
  let level = tcLevel ctx
  let levelsMap = tcLevelMap ctx
  let newIdent = makeIdent freshId
  put ctx {tcFreshId = freshId + 1, tcLevelMap = HashMap.insert newIdent level levelsMap}
  return (TUVar newIdent, newIdent)

freshUVar_ :: TypeInferencer n Type'
freshUVar_ = do
  (x, _ident) <- freshUVar
  return x

specialize :: Type' -> TypeInferencer n (Type', [Raw.UVarIdent])
specialize = \case
  TForAll (FoilTPatternVar binder) type_ -> do
    (x, ident) <- freshUVar
    let subst = Foil.addSubst Foil.identitySubst binder x
    (type', idents) <- specialize (FreeFoil.substitute Foil.emptyScope subst type_)
    return (type', ident : idents)
  type_ -> return (type_, [])

specialize_ :: Type' -> TypeInferencer n Type'
specialize_ t = do
  (t', _idents) <- specialize t
  return t'

generalize :: Type' -> TypeInferencer n Type'
generalize type_ = do
  ctx <- get
  let identsToQuantify =
        filter
          ( \ident -> case HashMap.lookup ident (tcLevelMap ctx) of
              Nothing -> error $ "Unification variable " ++ show ident ++ " not found in levels map"
              Just l -> l > tcLevel ctx
          )
          (Set.toList (freeVars type_))
  return $ generalizeWithIdents identsToQuantify type_

unify :: TypeInferencer n ()
unify = do
  ctx <- get
  (subst', levelsMap') <- fromEither $ unifyConstraintsWithSubst (tcLevelMap ctx) (tcConstraints ctx) (tcSubst ctx)
  let env' = applySubst subst' (tcEnv ctx)
  put
    ctx
      { tcConstraints = [],
        tcSubst = subst',
        tcLevelMap = levelsMap',
        tcEnv = env'
      }

-- | Alpha-equivalence for polytypes.
alphaEquivPoly :: Type' -> Type' -> TypeInferencer n Bool
alphaEquivPoly l r = do
  (l', xs) <- specialize $ genAll l
  (r', ys) <- specialize $ genAll r
  if length xs /= length ys
    then return False
    else do
      levelsMap <- gets tcLevelMap
      case unifyConstraint levelsMap (Constraint (l', r')) of
        Left _ -> return False
        Right (Subst subst, _) -> do
          let matchings = [(x, y) | (x, TUVar y) <- Map.toList subst]
              allXs = List.sort xs == List.sort (map fst matchings)
              allYs = List.sort ys == List.sort (map snd matchings)
          return (allXs && allYs)
  where
    genAll t = generalizeWithIdents (Set.toList (freeVars t)) t

-- | Log the current TypingContext using Debug.Trace.
logContext :: String -> TypeInferencer n ()
logContext msg = do
  TypingContext constraints subst env freshId levelMap level <- get
  TypeInferencer $ \tc ->
    trace
      ( "\n=== "
          ++ msg
          ++ " ===\n"
          ++ "Constraints: "
          ++ show constraints
          ++ "\n"
          ++ "Substitution: "
          ++ show subst
          ++ "\n"
          ++ "Fresh ID: "
          ++ show freshId
          ++ "\n"
          ++ "Current Level: "
          ++ show level
          ++ "\n"
          ++ "Levels Map: "
          ++ show levelMap
          ++ "\n"
          ++ "Typing Environment: "
          ++ show env
          ++ "\n"
          ++ "===================="
      )
      $ Right ((), tc)

--------------------------------------------------------------------------------

unifyConstraintsWithSubst :: IdentLevelMap -> [Constraint'] -> Subst' -> Either String (Subst', IdentLevelMap)
unifyConstraintsWithSubst levelsMap constrs subst = do
  (subst', levelsMap') <- unifyConstraints levelsMap (map (applySubst subst) constrs)
  return (composeSubst subst subst', levelsMap')

unifyConstraints :: IdentLevelMap -> [Constraint'] -> Either String (Subst', IdentLevelMap)
unifyConstraints levelsMap [] = return (nullSubst, levelsMap)
unifyConstraints levelsMap (c : cs) = do
  (subst', levelsMap') <- unifyConstraint levelsMap c
  (subst'', levelsMap'') <- unifyConstraintsWithSubst levelsMap' cs subst'
  return (subst'', levelsMap'')

unifyConstraint :: IdentLevelMap -> Constraint' -> Either String (Subst', IdentLevelMap)
unifyConstraint levelsMap (Constraint constr) =
  case constr of
    -- Case for unification variables
    (TUVar x, r) -> case r of
      TUVar y
        | x == y -> Right (nullSubst, levelsMap)
        | otherwise -> unifyWithUVar levelsMap x r
      _ -> unifyWithUVar levelsMap x r
    (l, TUVar x) -> unifyWithUVar levelsMap x l
    -- Case for Free Foil variables (not supported for now)
    (FreeFoil.Var x, FreeFoil.Var y)
      | x == y -> Left "unification of bound variables is not supported"
    -- Case of non-trivial arbitrary nodes
    (FreeFoil.Node l, FreeFoil.Node r) ->
      -- zipMatch (TArrowSig x1 x2) (TArrowSig y1 y2)
      --   = Just (TArrowSig (x1, y1) (x2, y2))
      case FreeFoil.zipMatch l r of
        Nothing -> Left ("cannot unify " ++ show constr)
        -- `zipMatch` takes out corresponding terms from a node that we need
        --  to unify further.
        Just lr -> unifyConstraints levelsMap (Constraint <$> F.toList lr) -- ignores "scopes", only works with "terms"
    (lhs, rhs) -> Left ("cannot unify " ++ show lhs ++ show rhs)

unifyWithUVar :: IdentLevelMap -> Raw.UVarIdent -> Type' -> Either String (Subst', IdentLevelMap)
unifyWithUVar levelsMap x type_ =
  if hasFreeVar x type_
    then Left "occurs check failed"
    else
      let updatedLevelsMap = case HashMap.lookup x levelsMap of
            Nothing -> Left "unification variable not found in levels map"
            Just xLevel ->
              Right $
                HashMap.unionWith
                  min
                  levelsMap
                  (HashMap.fromList [(var, xLevel) | var <- Set.toList (freeVars type_)])
       in case updatedLevelsMap of
            Left err -> Left err
            Right newLevelsMap -> Right (singleSubst x type_, newLevelsMap)

--------------------------------------------------------------------------------

makeIdent :: Int -> Raw.UVarIdent
makeIdent i = Raw.UVarIdent ("?u" ++ show i)

-- | Generalizes a type by wrapping it with forall binders for each free variable.
--
-- >>> generalizeWithIdents ["?a", "?b"] "?a -> ?b -> ?a"
-- forall x0 . (forall x1 . x0 -> x1 -> x0)
--
-- >>> generalizeWithIdents ["?y"] "?x -> ?y -> ?z"
-- forall x0 . ?x -> x0 -> ?z
generalizeWithIdents :: [Raw.UVarIdent] -> Type' -> Type'
generalizeWithIdents = go Foil.emptyScope
  where
    go :: (Foil.Distinct n) => Foil.Scope n -> [Raw.UVarIdent] -> Type n -> Type n
    go _ [] t = t
    go scope (x : xs) t = Foil.withFresh scope $ \binder ->
      let extendedScope = Foil.extendScope binder scope
          x' = FreeFoil.Var (Foil.nameOf binder)
          t' = applySubst (singleSubst x x') (Foil.sink t)
       in TForAll (FoilTPatternVar binder) (go extendedScope xs t')

--------------------------------------------------------------------------------

inferType :: Exp n -> TypeInferencer n Type'
inferType ETrue = return TBool
inferType EFalse = return TBool
inferType (ENat _) = return TNat
inferType (FreeFoil.Var x) = do
  TypingEnv env <- gets tcEnv
  specialize_ (Foil.lookupName x env)
inferType (ELet exprBinded (FoilPatternVar x) expr) = do
  bindedType <-
    enterLevel $
      inferType exprBinded
  unify
  subst <- gets tcSubst
  bindedTypeGeneral <-
    generalize $
      applySubst subst bindedType
  enterScope x bindedTypeGeneral $
    inferType expr
inferType (EAdd lhs rhs) = do
  lhsTyp <- inferType lhs
  rhsTyp <- inferType rhs
  addConstraints [(lhsTyp, TNat), (rhsTyp, TNat)]
  return TNat
inferType (ESub lhs rhs) = do
  lhsTyp <- inferType lhs
  rhsTyp <- inferType rhs
  addConstraints [(lhsTyp, TNat), (rhsTyp, TNat)]
  return TNat
inferType (EIf eCond eThen eElse) = do
  condTyp <- inferType eCond
  thenTyp <- inferType eThen
  elseTyp <- inferType eElse
  addConstraints [(condTyp, TBool), (thenTyp, elseTyp)]
  return thenTyp
inferType (EIsZero e) = do
  eTyp <- inferType e
  addConstraints [(eTyp, TNat)]
  return TBool
inferType (EAbs (FoilPatternVar x) eBody) = do
  paramType <- freshUVar_
  bodyTyp <-
    enterScope x paramType $
      inferType eBody
  return (TArrow paramType bodyTyp)
inferType (EApp eAbs eArg) = do
  absTyp <- inferType eAbs
  argTyp <- inferType eArg
  resultTyp <- freshUVar_
  addConstraints [(absTyp, TArrow argTyp resultTyp)]
  return resultTyp
inferType (ETyped e typ_) = do
  let typ = toTypeClosed typ_
  eTyp <- inferType e
  addConstraints [(eTyp, typ)]
  return typ
inferType (EFor eFrom eTo (FoilPatternVar x) eBody) = do
  fromTyp <- inferType eFrom
  toTyp <- inferType eTo
  addConstraints [(fromTyp, TNat), (toTyp, TNat)]
  enterScope x TNat $
    inferType eBody

--------------------------------------------------------------------------------

inferTypeClosed :: Exp Foil.VoidS -> Either String Type'
inferTypeClosed expr = do
  (type_, ctx) <- runTypeInferencer (inferType expr) initialTypingContext
  (subst, _levelsMap) <- unifyConstraintsWithSubst (tcLevelMap ctx) (tcConstraints ctx) (tcSubst ctx)
  return $ applySubst subst type_

--------------------------------------------------------------------------------
