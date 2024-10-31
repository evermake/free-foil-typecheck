module HM.InterpretSpec where

import Control.Monad (forM_)
import Data.List (sort)
import HM.Interpret
import System.Directory
import System.FilePath
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "well-typed expressions" $ do
    paths <- runIO (testFilesInDir "./test/files/well-typed")
    forM_ (sort paths) $ \path -> it path $ do
      contents <- readFile path
      interpret contents `shouldSatisfy` isSuccess

  describe "ill-typed expressions" $ do
    paths <- runIO (testFilesInDir "./test/files/ill-typed")
    forM_ (sort paths) $ \path -> it path $ do
      contents <- readFile path
      interpret contents `shouldSatisfy` isTypeError

isSuccess :: Result -> Bool
isSuccess Success {} = True
isSuccess _ = False

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
