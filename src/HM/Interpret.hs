{-# LANGUAGE DataKinds #-}

module HM.Interpret where

import Control.Monad.Foil (S (VoidS), emptyScope)
import HM.Eval
import HM.Parser.Par
import HM.Syntax (Exp, toExpClosed)
import HM.Typecheck

data Result
  = Success (Exp VoidS) -- Output of evaluation.
  | Failure ErrorKind String -- Error kind with message.
  deriving (Show)

data ErrorKind
  = ParsingError
  | TypecheckingError
  | EvaluationError
  deriving (Show)

interpret :: String -> Result
interpret input =
  case toExpClosed <$> pExp tokens of
    Left err -> Failure ParsingError ("Parsing error: " ++ err)
    Right e -> case inferTypeNewClosed e of
      Left err -> Failure TypecheckingError ("Typechecking error: " ++ err)
      Right _type -> case eval emptyScope e of
        Left err -> Failure EvaluationError ("Evaluation error: " ++ err)
        Right outExp -> Success outExp
  where
    tokens = myLexer input
