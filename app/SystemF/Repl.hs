{-# LANGUAGE DataKinds #-}

module Main where

import Control.Monad.Foil (S (VoidS), emptyNameMap)
import FreeFoilTypecheck.SystemF.Eval
import FreeFoilTypecheck.SystemF.Parser.Par
import FreeFoilTypecheck.SystemF.Syntax (Term (..), toTermClosed)
import FreeFoilTypecheck.SystemF.Typecheck

main :: IO ()
main = do
  putStrLn "Welcome to REPL!\n"
  interact (unlines . map repl . lines)

repl :: String -> String
repl input =
  case toTermClosed <$> pTerm tokens of
    Left err -> "Parsing error: " ++ err
    Right et@(Term e) -> case do
      checkInfer <- bidirectionaCheckInfer emptyNameMap e :: Either String (CheckInfer Term 'VoidS)
      infer checkInfer of
      Left err -> "Typechecking error: " ++ err
      Right _type -> case newEval emptyNameMap et of
        Left err -> "Evaluation error: " ++ err
        Right outExp -> show outExp
  where
    tokens = myLexer input
