{-# LANGUAGE DataKinds #-}

-- | Inference time of four engines on families of HM programs, measured with
-- Bodigrim's tasty-bench:
--
-- * the generic engine with level-based generalisation;
-- * the generic engine with naive generalisation (scans the environment);
-- * the language-specific engine ('FreeFoilTypecheck.HindleyMilner.Inference'),
--   which also uses levels;
-- * the generic engine with levels specialised by hand to the HM language
--   ('FreeFoilTypecheck.HindleyMilner.SpecializedInference').
--
-- The other three engines are compared ('bcompare') with the generic engine
-- with levels on the same program. Run with, e.g.,
-- @stack bench free-foil-typecheck:bench:generalization --benchmark-arguments='--csv bench.csv'@
-- and choose a family with @-p nested-let@.
module Main (main) where

import Control.DeepSeq (NFData (..))
import qualified Control.Monad.Free.Foil as FreeFoil
import Data.Bifoldable (Bifoldable, bifoldMap)
import Data.Monoid (Sum (..))
import FreeFoilTypecheck.GeneralTypecheck (Generalization (..), HMType (..), TypeScheme (..), UType, inferTypeSchemeClosed)
import qualified FreeFoilTypecheck.HindleyMilner.Inference as Specific
import FreeFoilTypecheck.HindleyMilner.Parser.Par (myLexer, pExp)
import FreeFoilTypecheck.HindleyMilner.Rules ()
import qualified FreeFoilTypecheck.HindleyMilner.SpecializedInference as Specialized
import FreeFoilTypecheck.HindleyMilner.Syntax (Exp', FoilTypePattern, TypeSig, toExpClosed)
-- (the instance of 'HMTypingSig' for the HM language comes from Rules)
import Test.Tasty.Bench
import Test.Tasty.Patterns.Printer (printAwkExpr)

-- | Number of nodes in an AST (forces the whole tree).
astSize :: (Bifoldable sig) => FreeFoil.AST binder sig n -> Int
astSize (FreeFoil.Var _) = 1
astSize (FreeFoil.Node node) =
  1 + getSum (bifoldMap (\(FreeFoil.ScopedAST _ body) -> Sum (astSize body)) (Sum . astSize) node)

data Engine = Engine
  { engineName :: String,
    -- | Size of the inferred type, or -1 on a type error.
    runEngine :: Exp' -> Int
  }

engines :: [Engine]
engines =
  [ Engine "generic-levels" (generic LevelBased),
    Engine "generic-naive" (generic Naive),
    Engine "specific-levels" (either (const (-1)) astSize . Specific.inferTypeClosed),
    Engine "specialized-levels" (either (const (-1)) astSize . Specialized.inferTypeClosed)
  ]
  where
    generic :: Generalization -> Exp' -> Int
    generic mode expr = case inferHM mode expr of
      Left _ -> -1
      Right (MonoType t) -> astSize t
      Right (PolyType (TypeScheme _ t)) -> astSize t

-- | The engine that the others are compared with.
baseline :: String
baseline = "generic-levels"

inferHM :: Generalization -> Exp' -> Either String (HMType (UType FoilTypePattern TypeSig))
inferHM = inferTypeSchemeClosed

data Family = Family
  { familyName :: String,
    familyProgram :: Int -> String,
    familySizes :: [Int]
  }

-- | @λa1. … λak. let f0 = λx. x in let f1 = λx. f0 x in … in fn ak@
lambdasThenLets :: Int -> Int -> String
lambdasThenLets k n =
  concat ["λa" ++ show i ++ ". " | i <- [1 .. k]]
    ++ "let f0 = λx. x in "
    ++ concat ["let f" ++ show i ++ " = λx. f" ++ show (i - 1) ++ " x in " | i <- [1 .. n]]
    ++ "f"
    ++ show n
    ++ " a"
    ++ show (max 1 k)

-- | @let f0 = λx. x in let f1 = λx. f0 (f0 x) in …@: every @let@ instantiates
-- the previous scheme twice.
letChain :: Int -> String
letChain n =
  "let f0 = λx. x in "
    ++ concat ["let f" ++ show i ++ " = λx. f" ++ show (i - 1) ++ " (f" ++ show (i - 1) ++ " x) in " | i <- [1 .. n]]
    ++ "f"
    ++ show n

-- | @nested-let@ has k = n outer λs, @wide-env@ has k = 500.
families :: [Family]
families =
  [ Family "nested-let" (\n -> lambdasThenLets n n) [160, 320, 640, 1280],
    Family "wide-env" (lambdasThenLets 500) [160, 320, 640],
    Family "let-chain" letChain [160, 320, 640, 1280]
  ]

-- | A parsed program. 'env' forces it before the measurement.
newtype Program = Program Exp'

instance NFData Program where
  rnf (Program expr) = rnf (astSize expr)

parseProgram :: String -> Program
parseProgram source = case pExp (myLexer source) of
  Left err -> error err
  Right raw -> Program (toExpClosed raw)

benchFamily :: Family -> Benchmark
benchFamily family = bgroup (familyName family) (map benchSize (familySizes family))
  where
    benchSize n =
      env (pure (parseProgram (familyProgram family n))) $ \program ->
        bgroup size (map (benchEngine size program) engines)
      where
        size = "n=" ++ show n
    benchEngine size program engine
      | engineName engine == baseline = measured
      | otherwise = bcompare (printAwkExpr (locateBenchmark [baseline, size, familyName family])) measured
      where
        measured = bench (engineName engine) (nf (\(Program expr) -> runEngine engine expr) program)

main :: IO ()
main = defaultMain (map benchFamily families)
