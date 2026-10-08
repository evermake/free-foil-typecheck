{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

-- | Hindley–Milner type inference for the HM language: the generic engine
-- ("FreeFoilTypecheck.GeneralTypecheck") with level-based generalisation,
-- specialised to this language by hand. The algorithm and the order of its
-- steps are those of the generic engine: integer unification variables
-- ('MetaVar') with their levels in a 'MetaVarMap', a triangular substitution resolved on demand
-- (@walk@) and zonked when a type is generalised, unification as inference
-- goes, and a typing environment that the substitution never rewrites. The
-- benchmark @generalization@ uses this module as a baseline for the cost of
-- genericity.
--
-- The types of the HM language ('Type') name unification variables with
-- strings and bind quantified variables with foil binders. Instead, the engine
-- uses a first-order type of its own ('UType') with integer unification
-- variables, and numbers the quantified variables of a scheme as @TGen@ in
-- Jones's /Typing Haskell in Haskell/. Only the final result is converted to
-- 'Type' ('schemeToType').
--
-- The sources of these techniques are those of the generic engine: Byrd for
-- triangular substitutions and @walk@, Peyton Jones et al. for zonking,
-- Romano's unification-fd for integer unification variables in an @IntMap@
-- (the module header of the generic engine has these references), and Rémy
-- for levels, in the presentation of Kiselyov. As in Kiselyov's
-- @sound_eager@, one traversal does both the occurs check and the level
-- adjustment (@bind@), and one traversal resolves and generalises a type
-- (@generalize@). The other references are:
--
-- * Didier Rémy. /Extension of ML type system with a sorted equation theory on
--   types/. Research Report RR-1766, INRIA, 1992.
--   <https://inria.hal.science/inria-00077006>
-- * Oleg Kiselyov. /How OCaml type checker works -- or what polymorphism and
--   garbage collection have in common/. 2013.
--   <https://okmij.org/ftp/ML/generalization.html>
-- * Mark P. Jones. /Typing Haskell in Haskell/. Haskell Workshop, 1999.
--   <https://web.cecs.pdx.edu/~mpj/thih/>
module FreeFoilTypecheck.HindleyMilner.SpecializedInference
  ( -- * Types
    UType (..),
    Scheme (..),

    -- * Inference
    inferSchemeClosed,
    inferTypeClosed,
    schemeToType,
  )
where

import Control.Monad (ap)
import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Free.Foil as FreeFoil
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified FreeFoilTypecheck.HindleyMilner.Parser.Abs as Raw
import FreeFoilTypecheck.HindleyMilner.Syntax
import FreeFoilTypecheck.MetaVar

-- $setup
-- >>> :set -XOverloadedStrings

-- * Types

-- | Types with unification variables.
data UType
  = UVar MetaVar
  | -- | The quantified variable with the given number, in a 'Scheme'.
    UGen Int
  | UNat
  | UBool
  | UArrow UType UType
  deriving (Eq, Show)

-- | A type scheme: the number of quantified variables and the type, in which
-- the quantified variables are @'UGen' 0@, @'UGen' 1@, and so on.
-- A monotype is a scheme without quantified variables.
data Scheme = Scheme Int UType
  deriving (Eq, Show)

-- * Inference monad

data InferState = InferState
  { -- | Triangular substitution of unification variables.
    stSubst :: MetaVarMap UType,
    -- | Level of each unification variable.
    stLevels :: MetaVarMap Level,
    -- | Next unification variable.
    stNext :: MetaVar
  }

newtype Infer a = Infer {runInfer :: InferState -> Either String (a, InferState)}
  deriving (Functor)

instance Applicative Infer where
  pure x = Infer $ \st -> Right (x, st)
  (<*>) = ap

instance Monad Infer where
  Infer g >>= f = Infer $ \st -> do
    (x, st') <- g st
    runInfer (f x) st'

get :: Infer InferState
get = Infer $ \st -> Right (st, st)

put :: InferState -> Infer ()
put st = Infer $ \_ -> Right ((), st)

failInfer :: String -> Infer a
failInfer msg = Infer $ \_ -> Left msg

-- | The level of a unification variable (each one gets a level when it is
-- created).
levelOf :: MetaVar -> InferState -> Level
levelOf x st = findWithDefaultMetaVarMap outermostLevel x (stLevels st)

-- | Create the given number of unification variables at a level, and return
-- the first of them (the others follow it, see 'blockVar').
freshVars :: Level -> Int -> Infer MetaVar
freshVars level count = do
  st <- get
  let first = stNext st
  put
    st
      { stNext = blockVar first count,
        stLevels = foldr (`insertMetaVarMap` level) (stLevels st) (take count (iterate nextMetaVar first))
      }
  return first

-- | The unification variable with the given index in a block of consecutive
-- variables that starts with the given one.
blockVar :: MetaVar -> Int -> MetaVar
blockVar (MetaVar first) i = MetaVar (first + i)

fresh :: Level -> Infer UType
fresh level = UVar <$> freshVars level 1

-- * Unification

-- | Resolve a bound unification variable at the root of a type.
walk :: MetaVarMap UType -> UType -> UType
walk subst type_@(UVar x) = maybe type_ (walk subst) (lookupMetaVarMap x subst)
walk _ type_ = type_

-- | Unify two types, extending the substitution.
unify :: UType -> UType -> Infer ()
unify type1 type2 = do
  subst <- stSubst <$> get
  case (walk subst type1, walk subst type2) of
    (UVar x, UVar y) | x == y -> return ()
    (UVar x, type_) -> bind x type_
    (type_, UVar x) -> bind x type_
    (UArrow a1 b1, UArrow a2 b2) -> unify a1 a2 >> unify b1 b2
    (UNat, UNat) -> return ()
    (UBool, UBool) -> return ()
    _ -> failInfer "cannot unify"

-- | Bind a unification variable to a type. One traversal of the type (through
-- the substitution) does the occurs check and lowers the levels of its
-- unification variables to the level of the bound variable.
bind :: MetaVar -> UType -> Infer ()
bind x type_ = do
  st <- get
  let level = levelOf x st
      adjust levels = \case
        UVar y -> case lookupMetaVarMap y (stSubst st) of
          Just bound -> adjust levels bound
          Nothing
            | y == x -> Nothing
            | otherwise -> Just (adjustMetaVarMap (min level) y levels)
        UArrow a b -> adjust levels a >>= (`adjust` b)
        _ -> Just levels
  case adjust (stLevels st) type_ of
    Nothing -> failInfer "occurs check failed"
    Just levels -> put st {stSubst = insertMetaVarMap x type_ (stSubst st), stLevels = levels}

-- * Generalisation and instantiation

-- | Generalise a type inferred one level deeper than the given one. In one
-- traversal, this applies the substitution and quantifies the unification
-- variables that are deeper than the given level, in the order of their first
-- occurrence. These variables cannot occur in the typing environment.
generalize :: Level -> UType -> Infer Scheme
generalize level type_ = do
  st <- get
  let quantify acc@(count, numbers) = \case
        UVar x -> case lookupMetaVarMap x (stSubst st) of
          Just bound -> quantify acc bound
          Nothing
            | levelOf x st <= level -> (UVar x, acc)
            | Just i <- lookupMetaVarMap x numbers -> (UGen i, acc)
            | otherwise -> (UGen count, (count + 1, insertMetaVarMap x count numbers))
        UArrow a b ->
          let (a', acc') = quantify acc a
              (b', acc'') = quantify acc' b
           in (UArrow a' b', acc'')
        t -> (t, acc)
      (body, (quantified, _)) = quantify (0, emptyMetaVarMap) type_
  return (Scheme quantified body)

-- | Instantiate the quantified variables of a scheme with fresh unification
-- variables, in one traversal.
instantiate :: Level -> Scheme -> Infer UType
instantiate _ (Scheme 0 type_) = return type_
instantiate level (Scheme count type_) = do
  first <- freshVars level count
  let go = \case
        UGen i -> UVar (blockVar first i)
        UArrow a b -> UArrow (go a) (go b)
        t -> t
  return (go type_)

-- * Inference

-- | Infer the type of a term at a level, in a typing environment.
infer :: Level -> Foil.NameMap n Scheme -> Exp n -> Infer UType
infer level env = \case
  FreeFoil.Var x -> instantiate level (Foil.lookupName x env)
  ETrue -> return UBool
  EFalse -> return UBool
  ENat _ -> return UNat
  EAdd l r -> do
    check l UNat
    check r UNat
    return UNat
  ESub l r -> do
    check l UNat
    check r UNat
    return UNat
  EIsZero arg -> do
    check arg UNat
    return UBool
  EIf cond then_ else_ -> do
    check cond UBool
    thenType <- infer level env then_
    check else_ thenType
    return thenType
  EApp fun arg -> do
    funType <- infer level env fun
    argType <- infer level env arg
    retType <- fresh level
    unify funType (UArrow argType retType)
    return retType
  ETyped term annotation -> do
    annotationType <- typeOfAnnotation level annotation
    check term annotationType
    return annotationType
  EFor from to (FoilPatternVar x) body -> do
    check from UNat
    check to UNat
    infer level (Foil.addNameBinder x (Scheme 0 UNat) env) body
  EAbs (FoilPatternVar x) body -> do
    paramType <- fresh level
    bodyType <- infer level (Foil.addNameBinder x (Scheme 0 paramType) env) body
    return (UArrow paramType bodyType)
  ELet bound (FoilPatternVar x) body -> do
    boundScheme <- infer (deeperLevel level) env bound >>= generalize level
    infer level (Foil.addNameBinder x boundScheme env) body
  where
    check term expected = do
      type_ <- infer level env term
      unify type_ expected

-- | The type of an annotation. Its unification variables (such as @?a@)
-- become fresh unification variables, in the order of their first occurrence.
typeOfAnnotation :: Level -> Raw.Type -> Infer UType
typeOfAnnotation level annotation = fst <$> go Map.empty annotation
  where
    go vars = \case
      Raw.TUVar x -> case Map.lookup x vars of
        Just type_ -> return (type_, vars)
        Nothing -> do
          type_ <- fresh level
          return (type_, Map.insert x type_ vars)
      Raw.TNat -> return (UNat, vars)
      Raw.TBool -> return (UBool, vars)
      Raw.TArrow a b -> do
        (a', vars') <- go vars a
        (b', vars'') <- go vars' b
        return (UArrow a' b', vars'')
      Raw.TVar (Raw.Ident x) -> failInfer ("unbound type variable in an annotation: " ++ x)
      Raw.TForAll _ _ -> failInfer "polymorphic type annotations are not supported"

-- | Infer the principal type scheme of a closed term.
inferSchemeClosed :: Exp' -> Either String Scheme
inferSchemeClosed expr =
  fst <$> runInfer (infer (deeperLevel outermostLevel) Foil.emptyNameMap expr >>= generalize outermostLevel) initialState
  where
    initialState = InferState {stSubst = emptyMetaVarMap, stLevels = emptyMetaVarMap, stNext = MetaVar 0}

-- | Infer the principal type scheme of a closed term, as a type of the HM
-- language with @forall@s (as 'FreeFoilTypecheck.HindleyMilner.Inference.inferTypeClosed').
--
-- >>> inferTypeClosed "let id = λx. x in id id"
-- Right forall x0 . x0 -> x0
-- >>> inferTypeClosed "λx. let y = x in y y"
-- Left "occurs check failed"
inferTypeClosed :: Exp' -> Either String Type'
inferTypeClosed expr = schemeToType <$> inferSchemeClosed expr

-- * Conversion to the HM language

-- | A scheme as a type of the HM language: a @forall@ for each quantified
-- variable (@'UGen' 0@ outermost), and @?uN@ for the unification variable @N@.
schemeToType :: Scheme -> Type'
schemeToType (Scheme count body) = quantify Foil.emptyScope (Names []) count
  where
    quantify :: (Foil.Distinct n) => Foil.Scope n -> Names n -> Int -> Type n
    quantify _ (Names names) 0 = fromUType (IntMap.fromList (zip [0 ..] (reverse names))) body
    quantify scope names k = Foil.withFresh scope $ \binder ->
      let Names names' = Foil.sink names
       in TForAll
            (FoilTPatternVar binder)
            (quantify (Foil.extendScope binder scope) (Names (Foil.nameOf binder : names')) (k - 1))

    fromUType :: IntMap.IntMap (Foil.Name n) -> UType -> Type n
    fromUType names = \case
      UVar (MetaVar x) -> TUVar (Raw.UVarIdent ("?u" ++ show x))
      UGen i -> FreeFoil.Var (names IntMap.! i)
      UNat -> TNat
      UBool -> TBool
      UArrow a b -> TArrow (fromUType names a) (fromUType names b)

-- | Names of quantified variables, the last one first.
-- Sinking it is a coercion ('Foil.sink').
newtype Names n = Names [Foil.Name n]

instance Foil.Sinkable Names where
  sinkabilityProof rename (Names names) = Names (map rename names)
