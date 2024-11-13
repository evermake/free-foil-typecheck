module HM.Interpret where

import Control.Monad.Foil (emptyNameMap)
import HM.Eval
import HM.Parser.Par
import HM.Syntax (toTermClosed)
import HM.Typecheck

data Result
  = Success String -- Output of evaluation.
  | Failure ErrorKind String -- Error kind with message.
  deriving (Show)

data ErrorKind
  = ParsingError
  | TypecheckingError
  | EvaluationError
  deriving (Show)

interpret :: String -> Result
interpret input =
  case toTermClosed <$> pTerm tokens of
    Left err -> Failure ParsingError ("Parsing error: " ++ err)
    Right e -> case inferType emptyNameMap e of
      Left err -> Failure TypecheckingError ("Typechecking error: " ++ err)
      Right _type -> case eval emptyNameMap e of
        Left err -> Failure EvaluationError ("Evaluation error: " ++ err)
        Right outExp -> Success (show outExp)
  where
    tokens = myLexer input
