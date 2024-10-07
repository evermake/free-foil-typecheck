-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for HM.

module HM.Parser.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified HM.Parser.Abs

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t, null spc, null rest) of
      (True , _   , True ) -> []             -- remove trailing space
      (False, _   , True ) -> t              -- remove trailing space
      (False, True, False) -> t ++ ' ' : s   -- add space if none
      _                    -> t ++ s
    where
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print HM.Parser.Abs.Ident where
  prt _ (HM.Parser.Abs.Ident i) = doc $ showString i
instance Print HM.Parser.Abs.UVarIdent where
  prt _ (HM.Parser.Abs.UVarIdent i) = doc $ showString i
instance Print HM.Parser.Abs.Pattern where
  prt i = \case
    HM.Parser.Abs.PatternVar id_ -> prPrec i 0 (concatD [prt 0 id_])

instance Print HM.Parser.Abs.Exp where
  prt i = \case
    HM.Parser.Abs.EVar id_ -> prPrec i 3 (concatD [prt 0 id_])
    HM.Parser.Abs.ETrue -> prPrec i 3 (concatD [doc (showString "true")])
    HM.Parser.Abs.EFalse -> prPrec i 3 (concatD [doc (showString "false")])
    HM.Parser.Abs.ENat n -> prPrec i 3 (concatD [prt 0 n])
    HM.Parser.Abs.EAdd exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "+"), prt 3 exp2])
    HM.Parser.Abs.ESub exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "-"), prt 3 exp2])
    HM.Parser.Abs.EIf exp1 exp2 exp3 -> prPrec i 1 (concatD [doc (showString "if"), prt 1 exp1, doc (showString "then"), prt 1 exp2, doc (showString "else"), prt 1 exp3])
    HM.Parser.Abs.EIsZero exp -> prPrec i 2 (concatD [doc (showString "iszero"), doc (showString "("), prt 0 exp, doc (showString ")")])
    HM.Parser.Abs.ETyped exp type_ -> prPrec i 0 (concatD [prt 1 exp, doc (showString ":"), prt 0 type_])
    HM.Parser.Abs.ELet pattern_ exp scopedexp -> prPrec i 1 (concatD [doc (showString "let"), prt 0 pattern_, doc (showString "="), prt 1 exp, doc (showString "in"), prt 0 scopedexp])
    HM.Parser.Abs.EAbs pattern_ type_ scopedexp -> prPrec i 1 (concatD [doc (showString "\955"), prt 0 pattern_, doc (showString ":"), prt 0 type_, doc (showString "."), prt 0 scopedexp])
    HM.Parser.Abs.EApp exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, prt 2 exp2])
    HM.Parser.Abs.ETAbs pattern_ scopedexp -> prPrec i 1 (concatD [doc (showString "\923"), prt 0 pattern_, doc (showString "."), prt 0 scopedexp])
    HM.Parser.Abs.ETApp exp type_ -> prPrec i 1 (concatD [prt 1 exp, doc (showString "["), prt 0 type_, doc (showString "]")])
    HM.Parser.Abs.EFor pattern_ exp1 exp2 scopedexp -> prPrec i 1 (concatD [doc (showString "for"), prt 0 pattern_, doc (showString "in"), doc (showString "["), prt 1 exp1, doc (showString ".."), prt 1 exp2, doc (showString "]"), doc (showString "do"), prt 0 scopedexp])

instance Print HM.Parser.Abs.ScopedExp where
  prt i = \case
    HM.Parser.Abs.ScopedExp exp -> prPrec i 0 (concatD [prt 1 exp])

instance Print HM.Parser.Abs.Type where
  prt i = \case
    HM.Parser.Abs.TUVar uvarident -> prPrec i 0 (concatD [prt 0 uvarident])
    HM.Parser.Abs.TNat -> prPrec i 0 (concatD [doc (showString "Nat")])
    HM.Parser.Abs.TBool -> prPrec i 0 (concatD [doc (showString "Bool")])
    HM.Parser.Abs.TArrow type_1 type_2 -> prPrec i 0 (concatD [prt 0 type_1, doc (showString "->"), prt 0 type_2])
    HM.Parser.Abs.TVar id_ -> prPrec i 0 (concatD [prt 0 id_])
    HM.Parser.Abs.TForAll pattern_ scopedtype -> prPrec i 0 (concatD [doc (showString "forall"), prt 0 pattern_, doc (showString "."), prt 0 scopedtype])

instance Print HM.Parser.Abs.ScopedType where
  prt i = \case
    HM.Parser.Abs.ScopedType type_ -> prPrec i 0 (concatD [prt 0 type_])
