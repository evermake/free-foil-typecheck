module FreeFoilTypecheck.HindleyMilner.InferenceSpec where

import Control.Monad (forM_)
import Data.List
import qualified Data.Set as Set
import FreeFoilTypecheck.HindleyMilner.Inference (alphaEquivPolyTypes, evalTypeInferencer, freeVars, generalizeWithIdents, inferTypeClosed)
import FreeFoilTypecheck.HindleyMilner.Interpret
import FreeFoilTypecheck.HindleyMilner.Parser.Par (myLexer, pExp, pType)
import FreeFoilTypecheck.HindleyMilner.Syntax (toExpClosed, toTypeClosed)
import System.Directory
import System.FilePath
import Test.Hspec

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import FreeFoilTypecheck.HindleyMilner.Syntax (Type', Exp')

spec :: Spec
spec = parallel $ do
  describe "well-typed expressions" $ do
    paths <- runIO (testFilesInDir "./test/FreeFoilTypecheck/HindleyMilner/files/well-typed")
    forM_ (sort (filter (\p -> not (".expected.lam" `isSuffixOf` p)) paths)) $ \path -> it path $ do
      contents <- readFile path
      expectedTypeContents <- readFile (replaceExtension path ".expected.lam")
      expTypeMatches contents expectedTypeContents `shouldBe` Right True

  describe "ill-typed expressions" $ do
    paths <- runIO (testFilesInDir "./test/FreeFoilTypecheck/HindleyMilner/files/ill-typed")
    forM_ (sort paths) $ \path -> it path $ do
      contents <- readFile path
      interpret contents `shouldSatisfy` isTypeError

isTypeError :: Result -> Bool
isTypeError (Failure TypecheckingError _) = True
isTypeError _ = False

testFilesInDir :: FilePath -> IO [FilePath]
testFilesInDir dir = do
  let isTestFile f = return $ takeExtension f == ".lam"
  dirWalk isTestFile dir

dirWalk :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
dirWalk filefunc top = do
  isDirectory <- doesDirectoryExist top
  if isDirectory
    then do
      -- Files preserving full path with `top`
      files <- map (top </>) <$> listDirectory top
      paths <- mapM (dirWalk filefunc) files
      return $ concat paths
    else do
      included <- filefunc top
      return ([top | included])

-- | Checks whether the expression matches the expected type by parsing
-- the expression source code and inferring its' type, then parsing
-- the expected type source code and comparing the obtained types.
--
-- Using `FreeFoil.alphaEquiv` wouldn't work as expected, as it only compares
-- the AST structure and `FreeFoil.Var`s, and doesn't take into account values
-- of the literals:
--
-- >>> FreeFoil.alphaEquiv Foil.emptyScope ("?a -> ?a" :: Type') ("?a -> ?b" :: Type')
-- True
-- >>> FreeFoil.alphaEquiv Foil.emptyScope ("1" :: Exp') ("2" :: Exp')
-- True
--
-- We could generalize all type variables and compare the generalized types,
-- however, this would not work either as the order of generalization is not
-- determined, and though order of quantifiers doesn't change the meaning,
-- it does affect alpha equivalence:
--
-- >>> FreeFoil.alphaEquiv Foil.emptyScope ("forall x. (forall y. x -> y)" :: Type') ("forall y. (forall x. x -> y)" :: Type')
-- False
--
-- Therefore, we should use the `alphaEquivPolyTypes` function to compare the
-- types.
expTypeMatches :: String -> String -> Either String Bool
expTypeMatches expSource expectedTypeSource = do
  -- parse
  expr <- toExpClosed <$> pExp (myLexer expSource)
  expectedType <- toTypeClosed <$> pType (myLexer expectedTypeSource)
  -- infer
  actualType <- inferTypeClosed expr
  -- generalize
  let expectedType' = generalizeWithIdents (Set.toList (freeVars expectedType)) expectedType
  let actualType' = generalizeWithIdents (Set.toList (freeVars actualType)) actualType
  -- compare
  case evalTypeInferencer $ alphaEquivPolyTypes expectedType' actualType' of
    Left err -> Left err
    Right True -> Right True
    Right False ->
      Left $
        unlines
          [ "types do not match",
            "expected:",
            show expectedType,
            "but actual is:",
            show actualType
          ]
