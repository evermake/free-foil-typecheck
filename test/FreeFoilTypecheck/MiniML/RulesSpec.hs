-- | Runs the generic engine on the MiniML test programs.
--
-- The programs are standard examples (@map@, @foldr@, @append@, @zip@,
-- let-polymorphism of @id@). Two of them test what programs of Kiselyov in
-- the Hindley–Milner suite test: @011_self_application@ (as @kiselyov_07@) and
-- @018_lambda_let_levels@ (as @kiselyov_14@ and @kiselyov_18@), and
-- @023_let_pattern_monomorphic@ tests the same for a pattern bound by @let@.
-- The well-typed programs from 025 and the ill-typed ones from 019 exercise
-- patterns: nested patterns, wildcards, patterns of λ, @let@, @letrec@ and
-- @fix@ (mutual recursion through a pair, as in Mini-ML), @case@ with more
-- than two branches, a @case@ nested in the first branch of another,
-- refutable patterns of λ and @let@, and a variable bound twice by a pattern,
-- which Mini-ML rejects (@021_duplicate_binder@ is its example).
module FreeFoilTypecheck.MiniML.RulesSpec where

import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.List (isSuffixOf, sort)
import FreeFoilTypecheck.GeneralTypecheck (Generalization (..), HMType (..), canonicalHMType, equivUpToRenaming)
import FreeFoilTypecheck.HindleyMilner.InferenceSpec (dirWalk)
import FreeFoilTypecheck.MiniML.Parser.Par (myLexer, pExp, pType)
import FreeFoilTypecheck.MiniML.Rules (fromTypeClosed, inferMiniML, showHMType)
import FreeFoilTypecheck.MiniML.Syntax (toTypeClosed)
import System.FilePath (replaceExtension, takeExtension)
import Test.Hspec

spec :: Spec
spec = parallel $ forM_ [LevelBased, Naive] $ \generalization -> do
  describe ("well-typed MiniML programs (" ++ show generalization ++ ")") $ do
    paths <- runIO (miniMLFilesInDir "./test/FreeFoilTypecheck/MiniML/files/well-typed")
    forM_ (sort (filter (\p -> not (".expected.ml" `isSuffixOf` p)) paths)) $ \path -> it path $ do
      contents <- readFile path
      expectedTypeContents <- readFile (replaceExtension path ".expected.ml")
      miniMLTypeMatches generalization contents expectedTypeContents `shouldBe` Right True

  describe ("ill-typed MiniML programs (" ++ show generalization ++ ")") $ do
    paths <- runIO (miniMLFilesInDir "./test/FreeFoilTypecheck/MiniML/files/ill-typed")
    forM_ (sort paths) $ \path -> it path $ do
      contents <- readFile path
      -- every test program must parse
      _ <- either (fail . ("parsing error: " ++)) return (pExp (myLexer contents))
      (showHMType <$> inferMiniML generalization contents) `shouldSatisfy` isLeft

miniMLFilesInDir :: FilePath -> IO [FilePath]
miniMLFilesInDir = dirWalk (\f -> return (takeExtension f == ".ml"))

-- | Whether the inferred type scheme is the expected one (up to renaming).
miniMLTypeMatches :: Generalization -> String -> String -> Either String Bool
miniMLTypeMatches generalization source expectedSource = do
  expected <- fromTypeClosed . toTypeClosed <$> pType (myLexer expectedSource)
  actual <- canonicalHMType <$> inferMiniML generalization source
  if equivUpToRenaming actual (MonoType expected)
    then Right True
    else
      Left $
        unlines
          [ "types do not match",
            "expected:",
            show expected,
            "but actual is:",
            showHMType actual
          ]
