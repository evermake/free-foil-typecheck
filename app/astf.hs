module Main where

import Control.Monad.Foil (emptyScope)

-- import HM.Eval
-- import HM.Parser.Par
-- import HM.Syntax (toExpClosed)
-- import HM.Typecheck

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

sizeOfType :: Type n -> Int
