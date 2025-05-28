{-# LANGUAGE DataKinds #-}

module Main where

import Control.Monad.Foil (S (VoidS), emptyNameMap)
import FreeFoilTypecheck.SystemF.Eval
import FreeFoilTypecheck.SystemF.Parser.Par
import FreeFoilTypecheck.SystemF.Syntax (Term (..), toTermClosed)
import FreeFoilTypecheck.SystemF.Typecheck
import FreeFoilTypecheck.SystemF.TypecheckGen
import FreeFoilTypecheck.SystemF.TypingSig ()
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-gen"] -> do
      putStrLn "Welcome to REPL! (gen)\n"
      interact (unlines . map genRepl . lines)
    _ -> do
      putStrLn "Welcome to REPL!\n"
      interact (unlines . map repl . lines)

repl :: String -> String
repl input =
  case toTermClosed <$> pTerm tokens of
    Left err -> "Parsing error: " ++ err
    Right e -> case inferType emptyNameMap e of
      Left err -> "Typechecking error: " ++ err
      Right _type -> case newEval emptyNameMap e of
        Left err -> "Evaluation error: " ++ err
        Right outExp -> show outExp
  where
    tokens = myLexer input

genRepl :: String -> String
genRepl input =
  case toTermClosed <$> pTerm tokens of
    Left err -> "Parsing error: " ++ err
    Right et@(Term e) -> case do
      checkInfer <- bidirectionalCheckInfer emptyNameMap e :: Either String (CheckInfer Term 'VoidS)
      infer checkInfer of
      Left err -> "Typechecking error: " ++ err
      Right _type -> case newEval emptyNameMap et of
        Left err -> "Evaluation error: " ++ err
        Right outExp -> show outExp
  where
    tokens = myLexer input
