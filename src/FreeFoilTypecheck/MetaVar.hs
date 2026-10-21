-- | Unification variables, finite maps and sets of them, and levels of
-- generalisation, shared by the generic engine
-- ("FreeFoilTypecheck.GeneralTypecheck") and the hand-specialised engine
-- ("FreeFoilTypecheck.HindleyMilner.SpecializedInference").
--
-- Unification variables are integers, and maps and sets of them are an
-- 'IntMap.IntMap' and an 'IntSet.IntSet', as in the module
-- @Control.Unification.IntVar@ of Wren Romano's unification-fd
-- (<https://hackage.haskell.org/package/unification-fd>).
module FreeFoilTypecheck.MetaVar
  ( -- * Unification variables
    MetaVar (..),
    nextMetaVar,

    -- * Maps from unification variables
    MetaVarMap,
    emptyMetaVarMap,
    fromListMetaVarMap,
    lookupMetaVarMap,
    findWithDefaultMetaVarMap,
    insertMetaVarMap,
    sizeMetaVarMap,

    -- * Sets of unification variables
    MetaVarSet,
    emptyMetaVarSet,
    insertMetaVarSet,
    fromListMetaVarSet,
    memberMetaVarSet,

    -- * Levels
    Level (..),
    outermostLevel,
    deeperLevel,
    shallowerLevel,
    lowerLevel,
  )
where

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

-- | A unification variable (metavariable).
newtype MetaVar = MetaVar Int
  deriving (Eq, Ord, Show)

-- | The unification variable created after the given one.
nextMetaVar :: MetaVar -> MetaVar
nextMetaVar (MetaVar i) = MetaVar (i + 1)

-- | A finite map from unification variables.
newtype MetaVarMap a = MetaVarMap (IntMap.IntMap a)

emptyMetaVarMap :: MetaVarMap a
emptyMetaVarMap = MetaVarMap IntMap.empty

fromListMetaVarMap :: [(MetaVar, a)] -> MetaVarMap a
fromListMetaVarMap xs = MetaVarMap (IntMap.fromList [(i, a) | (MetaVar i, a) <- xs])

lookupMetaVarMap :: MetaVar -> MetaVarMap a -> Maybe a
lookupMetaVarMap (MetaVar i) (MetaVarMap m) = IntMap.lookup i m

findWithDefaultMetaVarMap :: a -> MetaVar -> MetaVarMap a -> a
findWithDefaultMetaVarMap def (MetaVar i) (MetaVarMap m) = IntMap.findWithDefault def i m

insertMetaVarMap :: MetaVar -> a -> MetaVarMap a -> MetaVarMap a
insertMetaVarMap (MetaVar i) a (MetaVarMap m) = MetaVarMap (IntMap.insert i a m)

sizeMetaVarMap :: MetaVarMap a -> Int
sizeMetaVarMap (MetaVarMap m) = IntMap.size m

-- | A finite set of unification variables.
newtype MetaVarSet = MetaVarSet IntSet.IntSet

emptyMetaVarSet :: MetaVarSet
emptyMetaVarSet = MetaVarSet IntSet.empty

insertMetaVarSet :: MetaVar -> MetaVarSet -> MetaVarSet
insertMetaVarSet (MetaVar i) (MetaVarSet set) = MetaVarSet (IntSet.insert i set)

fromListMetaVarSet :: [MetaVar] -> MetaVarSet
fromListMetaVarSet xs = MetaVarSet (IntSet.fromList [i | MetaVar i <- xs])

memberMetaVarSet :: MetaVar -> MetaVarSet -> Bool
memberMetaVarSet (MetaVar i) (MetaVarSet set) = IntSet.member i set

-- | A level of generalisation: the number of enclosing generalisations (of
-- the bound terms of @let@s, and of the whole term).
newtype Level = Level Int
  deriving (Eq, Ord, Show)

-- | The level outside of any generalisation.
outermostLevel :: Level
outermostLevel = Level 0

-- | The level inside one more generalisation.
deeperLevel :: Level -> Level
deeperLevel (Level n) = Level (n + 1)

-- | The level outside of the innermost generalisation.
shallowerLevel :: Level -> Level
shallowerLevel (Level n) = Level (n - 1)

-- | @lowerLevel level x levels@ gives @x@ the given level if its own level is
-- deeper. The map is left as it is otherwise, which avoids rebuilding it when
-- no level changes.
lowerLevel :: Level -> MetaVar -> MetaVarMap Level -> MetaVarMap Level
lowerLevel level x levels = case lookupMetaVarMap x levels of
  Just level' | level' > level -> insertMetaVarMap x level levels
  _ -> levels
