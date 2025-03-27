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

spec :: Spec
spec = parallel $ do
  describe "well-typed expressions" $ do
    paths <- runIO (testFilesInDir "./test/FreeFoilTypecheck/HindleyMilner/files/well-typed")
    forM_ (sort (filter (\p -> not (".expected.lam" `isSuffixOf` p)) paths)) $ \path -> it path $ do
      contents <- readFile path
      expectedTypeContents <- readFile (replaceExtension path ".expected.lam")
      programTypesMatch contents expectedTypeContents `shouldBe` Right True

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

programTypesMatch :: String -> String -> Either String Bool
programTypesMatch actual expected = do
  typeExpected <- toTypeClosed <$> pType tokensExpected
  let typeExpectedGeneral = generalizeWithIdents (Set.toList (freeVars typeExpected)) typeExpected
  exprActual <- toExpClosed <$> pExp tokensActual
  typeActual <- inferTypeClosed exprActual
  let typeActualGeneral = generalizeWithIdents (Set.toList (freeVars typeActual)) typeActual
  case evalTypeInferencer $ alphaEquivPolyTypes typeActualGeneral typeExpectedGeneral of
    Left err -> Left err
    Right True -> Right True
    Right False ->
      Left $
        unlines
          [ "types do not match",
            "expected:",
            show typeExpected,
            "but actual is:",
            show typeActual
          ]
  where
    tokensActual = myLexer actual
    tokensExpected = myLexer expected
