module Main where

import Control.Monad.Foil (emptyScope)
import FreeFoilTypecheck.HindleyMilner.Eval
import FreeFoilTypecheck.HindleyMilner.Parser.Par
import FreeFoilTypecheck.HindleyMilner.Syntax (toExpClosed)
import FreeFoilTypecheck.HindleyMilner.Typecheck

main :: IO ()
main = do
  putStrLn "Welcome to REPL!\n"
  interact (unlines . map repl . lines)

repl :: String -> String
repl input =
  case toExpClosed <$> pExp tokens of
    Left err -> "Parsing error: " ++ err
    Right e -> case inferTypeNewClosed e of
      Left err -> "Typechecking error: " ++ err
      Right _type -> case eval emptyScope e of
        Left err -> "Evaluation error: " ++ err
        Right outExp -> show outExp
  where
    tokens = myLexer input
