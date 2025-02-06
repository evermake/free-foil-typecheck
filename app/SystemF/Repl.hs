module Main where

import           Control.Monad.Foil (emptyNameMap)
import           FreeFoilTypecheck.SystemF.Eval
import           FreeFoilTypecheck.SystemF.Parser.Par
import           FreeFoilTypecheck.SystemF.Syntax          (toTermClosed)
import           FreeFoilTypecheck.SystemF.Typecheck

main :: IO ()
main = do
  putStrLn "Welcome to REPL!\n"
  interact (unlines . map repl . lines)

repl :: String -> String
repl input =
  case toTermClosed <$> pTerm tokens of
    Left err -> "Parsing error: " ++ err
    Right e -> case inferType emptyNameMap e of
      Left err -> "Typechecking error: " ++ err
      Right _type -> case eval emptyNameMap e of
        Left err     -> "Evaluation error: " ++ err
        Right outExp -> show outExp
  where
    tokens = myLexer input
