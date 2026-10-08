{-# LANGUAGE DataKinds #-}

-- | Inference time of three engines on families of HM programs:
--
-- * the generic engine with level-based generalisation;
-- * the generic engine with naive generalisation (scans the environment);
-- * the language-specific engine ('FreeFoilTypecheck.HindleyMilner.Inference'),
--   which also uses levels.
--
-- Usage: @nested-let [REPETITIONS [BUDGET_SECONDS]]@.
-- Prints CSV: family, engine, n, median, minimum and maximum time in milliseconds.
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM, forM_, when)
import qualified Control.Monad.Free.Foil as FreeFoil
import Data.Bifoldable (Bifoldable, bifoldMap)
import Data.IORef
import Data.List (sort)
import Data.Monoid (Sum (..))
import FreeFoilTypecheck.GeneralTypecheck (Generalization (..), HMType (..), TypeScheme (..), UType, inferTypeSchemeClosed)
import qualified FreeFoilTypecheck.HindleyMilner.Inference as Specific
import FreeFoilTypecheck.HindleyMilner.Parser.Par (myLexer, pExp)
import FreeFoilTypecheck.HindleyMilner.Rules ()
import FreeFoilTypecheck.HindleyMilner.Syntax (Exp', FoilTypePattern, TypeSig, toExpClosed)
-- (the instance of 'HMTypingSig' for the HM language comes from Rules)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs)
import System.IO
import Text.Printf (printf)

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
    Engine "specific-levels" (either (const (-1)) astSize . Specific.inferTypeClosed)
  ]
  where
    generic :: Generalization -> Exp' -> Int
    generic mode expr = case inferHM mode expr of
      Left _ -> -1
      Right (MonoType t) -> astSize t
      Right (PolyType (TypeScheme _ t)) -> astSize t

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

families :: [Family]
families =
  [ Family "nested-let (k = n)" (\n -> lambdasThenLets n n) [10, 20, 40, 80, 160, 320, 640, 1280],
    Family "wide-env (k = 500)" (lambdasThenLets 500) [10, 20, 40, 80, 160, 320, 640],
    Family "let-chain (k = 0)" letChain [10, 20, 40, 80, 160, 320, 640, 1280]
  ]

parseProgram :: String -> Exp'
parseProgram source = case pExp (myLexer source) of
  Left err -> error err
  Right raw -> toExpClosed raw

-- | Time one run in milliseconds. The term is built and forced first.
timeRun :: Engine -> Family -> Int -> IO (Double, Int)
timeRun engine family n = do
  expr <- evaluate (parseProgram (familyProgram family n))
  _ <- evaluate (astSize expr)
  t0 <- getMonotonicTimeNSec
  result <- evaluate (runEngine engine expr)
  t1 <- getMonotonicTimeNSec
  return (fromIntegral (t1 - t0) / 1e6, result)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let repetitions = case args of
        r : _ -> read r
        [] -> 7
      budget = case args of
        _ : b : _ -> read b
        _ -> 20 :: Double
  putStrLn "family,engine,n,median_ms,min_ms,max_ms,repetitions,result_size"
  forM_ families $ \family ->
    forM_ engines $ \engine -> do
      stop <- newIORef False
      forM_ (familySizes family) $ \n -> do
        stopped <- readIORef stop
        when (not stopped) $ do
          runs <- forM [1 .. repetitions :: Int] $ \_ -> timeRun engine family n
          let times = sort (map fst runs)
              median = times !! (length times `div` 2)
              resultSize = case runs of
                (_, size) : _ -> size
                [] -> 0
          printf
            "%s,%s,%d,%.3f,%.3f,%.3f,%d,%d\n"
            (familyName family)
            (engineName engine)
            n
            median
            (minimum times)
            (maximum times)
            repetitions
            resultSize
          -- skip larger sizes once the repetitions of one size exceed the budget
          when (median * fromIntegral repetitions > budget * 1000) $ writeIORef stop True
