{-# LANGUAGE DataKinds #-}

module FreeFoilTypecheck.HindleyMilner.Interpret where

import Control.Monad.Foil (S (VoidS), emptyScope)
import FreeFoilTypecheck.HindleyMilner.Eval
import FreeFoilTypecheck.HindleyMilner.Parser.Par
import FreeFoilTypecheck.HindleyMilner.Syntax (Exp, Type', toExpClosed)
import FreeFoilTypecheck.HindleyMilner.Typecheck

data Result
  = Success (Exp VoidS, Type') -- Output of evaluation.
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
      Right type_ -> case eval emptyScope e of
        Left err -> Failure EvaluationError ("Evaluation error: " ++ err)
        Right outExp -> Success (outExp, type_)
  where
    tokens = myLexer input
