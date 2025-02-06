module Main where

import FreeFoilTypecheck.HindleyMilner.Interpret
import System.Exit

main :: IO ()
main = do
  sourceCode <- getContents
  case interpret sourceCode of
    Success (outExpr, _) -> putStrLn (show outExpr)
    Failure errorKind errorMsg -> do
      putStrLn errorMsg
      exitWith (ExitFailure (errorCode errorKind))

errorCode :: ErrorKind -> Int
errorCode ParsingError = 1
errorCode TypecheckingError = 2
errorCode EvaluationError = 3
