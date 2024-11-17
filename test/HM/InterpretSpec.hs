module HM.InterpretSpec where

import Control.Monad (forM_)
import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Free.Foil as Foil
import Data.List
import HM.Interpret
import HM.Parser.Par (myLexer, pExp, pType)
import HM.Syntax (toExpClosed, toTypeClosed)
import HM.Typecheck (inferTypeNewClosed)
import System.Directory
import System.FilePath
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "well-typed expressions" $ do
    paths <- runIO (testFilesInDir "./test/files/well-typed")
    forM_ (sort (filter (\p -> not (".expected.lam" `isSuffixOf` p)) paths)) $ \path -> it path $ do
      contents <- readFile path
      expectedTypeContents <- readFile (replaceExtension path ".expected.lam")
      programTypesMatch contents expectedTypeContents `shouldBe` Right True

  describe "ill-typed expressions" $ do
    paths <- runIO (testFilesInDir "./test/files/ill-typed")
    forM_ (sort paths) $ \path -> it path $ do
      contents <- readFile path
      interpret contents `shouldSatisfy` isTypeError

isTypeError :: Result -> Bool
isTypeError (Failure TypecheckingError _) = True
isTypeError _ = False

testFilesInDir :: FilePath -> IO [FilePath]
testFilesInDir dir = do
  let isTestFile = \f -> return $ takeExtension f == ".lam"
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
      return $
        if included
          then [top]
          else []

programTypesMatch :: String -> String -> Either String Bool
programTypesMatch actual expected = do
  typeExpected <- toTypeClosed <$> pType tokensExpected
  exprActual <- toExpClosed <$> pExp tokensActual
  typeActual <- inferTypeNewClosed exprActual
  case (Foil.alphaEquiv Foil.emptyScope typeActual typeExpected) of
    True -> Right True
    False ->
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
