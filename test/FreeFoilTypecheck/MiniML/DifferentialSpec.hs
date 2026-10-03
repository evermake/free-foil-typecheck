-- | Differential test for MiniML: level-based and naive generalisation
-- must agree on random closed terms.
module FreeFoilTypecheck.MiniML.DifferentialSpec where

import FreeFoilTypecheck.GeneralTypecheck (Generalization (..), equivUpToRenaming, inferTypeSchemeClosed)
import qualified FreeFoilTypecheck.MiniML.Parser.Abs as Raw
import FreeFoilTypecheck.MiniML.Parser.Print (printTree)
import FreeFoilTypecheck.MiniML.Rules (showHMType)
import FreeFoilTypecheck.MiniML.Syntax (toExpClosed)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck

spec :: Spec
spec =
  describe "level-based and naive generalisation agree on random MiniML terms" $
    modifyMaxSuccess (const 3000) $ do
      prop "levels = naive (all constructs)" $
        forAll (sized (genExp [])) agreeOn
      prop "levels = naive (λ, application, let and letrec only)" $
        forAll (sized (genPureExp [])) agreeOn

agreeOn :: Raw.Exp -> Property
agreeOn raw =
  let expr = toExpClosed raw
      levels = inferTypeSchemeClosed LevelBased expr
      naive = inferTypeSchemeClosed Naive expr
      agree = case (levels, naive) of
        (Left _, Left _) -> True
        (Right t1, Right t2) -> equivUpToRenaming t1 t2
        _ -> False
   in classify (either (const False) (const True) levels) "well-typed" $
        counterexample (printTree raw) $
          counterexample (either ("error: " ++) showHMType levels) $
            counterexample (either ("error: " ++) showHMType naive) agree

-- | Random closed terms with variables, λ, application, let and letrec only.
genPureExp :: [Raw.Ident] -> Int -> Gen Raw.Exp
genPureExp scope size
  | size <= 1 || null scope = if null scope then binder Raw.EAbs else variable
  | otherwise =
      frequency
        [ (2, variable),
          (3, binder Raw.EAbs),
          (4, Raw.EApp <$> genPureExp scope (size `div` 2) <*> genPureExp scope (size `div` 2)),
          (3, letExpression),
          (1, letRec)
        ]
  where
    variable = Raw.EVar <$> elements scope
    name = elements (map Raw.Ident ["x", "y", "z", "f", "g"])
    binder con = do
      x <- name
      con (Raw.PatternVar x) . Raw.ScopedExp <$> genPureExp (x : scope) (size - 1)
    letExpression = do
      x <- name
      bound <- genPureExp scope (size `div` 2)
      body <- genPureExp (x : scope) (size `div` 2)
      return (Raw.ELet (Raw.PatternVar x) bound (Raw.ScopedExp body))
    letRec = do
      x <- name
      bound <- genPureExp (x : scope) (size `div` 2)
      body <- genPureExp (x : scope) (size `div` 2)
      return (Raw.ELetRec (Raw.PatternVar x) (Raw.ScopedExp bound) (Raw.ScopedExp body))

-- | Random closed MiniML terms (without annotations). Binders get random
-- patterns ('genPattern').
genExp :: [Raw.Ident] -> Int -> Gen Raw.Exp
genExp scope size
  | size <= 1 = leaf
  | otherwise =
      frequency
        [ (2, leaf),
          (3, binder Raw.EAbs),
          (4, Raw.EApp <$> sub 2 <*> sub 2),
          (3, letExpression),
          (1, letRec),
          (1, binder Raw.EFix),
          (1, Raw.EPair <$> sub 2 <*> sub 2),
          (1, elements [Raw.EFst, Raw.ESnd, Raw.EInl, Raw.EInr] <*> sub 1),
          (1, Raw.ECons <$> sub 2 <*> sub 2),
          (2, caseExpression),
          (1, Raw.EIf <$> sub 3 <*> sub 3 <*> sub 3),
          (1, Raw.EAdd <$> sub 2 <*> sub 2)
        ]
  where
    sub k = genExp scope (size `div` k)
    leaf
      | null scope = constant
      | otherwise = frequency [(4, Raw.EVar <$> elements scope), (1, constant)]
    constant = elements [Raw.ETrue, Raw.EFalse, Raw.ENat 0, Raw.ENat 1, Raw.ENil]
    binder con = do
      (pat, bound) <- genPattern
      con pat . Raw.ScopedExp <$> genExp (bound ++ scope) (size - 1)
    letExpression = do
      (pat, bound) <- genPattern
      e1 <- genExp scope (size `div` 2)
      e2 <- genExp (bound ++ scope) (size `div` 2)
      return (Raw.ELet pat e1 (Raw.ScopedExp e2))
    letRec = do
      (pat, bound) <- genPattern
      e1 <- genExp (bound ++ scope) (size `div` 2)
      e2 <- genExp (bound ++ scope) (size `div` 2)
      return (Raw.ELetRec pat (Raw.ScopedExp e1) (Raw.ScopedExp e2))
    caseExpression = do
      n <- choose (1, 3)
      scrutinee <- sub (n + 1)
      branches <- vectorOf n $ do
        (pat, bound) <- genPattern
        body <- genExp (bound ++ scope) (size `div` (n + 1))
        return (Raw.Branch pat (Raw.ScopedExp body))
      return (Raw.ECase scrutinee branches)

-- | A random pattern and the variables it binds. A pattern binds each of the
-- names @x@, @y@, @z@, @f@ and @g@ at most once.
genPattern :: Gen (Raw.Pattern, [Raw.Ident])
genPattern = do
  names <- shuffle (map Raw.Ident ["x", "y", "z", "f", "g"])
  size <- choose (1, 4)
  (pat, unused) <- go size names
  return (pat, take (length names - length unused) names)
  where
    go :: Int -> [Raw.Ident] -> Gen (Raw.Pattern, [Raw.Ident])
    go size names
      | size <= 1 = leaf names
      | otherwise =
          frequency
            [ (2, leaf names),
              (1, binary Raw.PatternPair size names),
              (1, binary Raw.PatternCons size names),
              (1, unary Raw.PatternInl size names),
              (1, unary Raw.PatternInr size names)
            ]
    leaf names = case names of
      x : rest -> frequency [(4, return (Raw.PatternVar x, rest)), (1, return (Raw.PatternWildcard, names)), (1, return (Raw.PatternNil, names))]
      [] -> elements [(Raw.PatternWildcard, names), (Raw.PatternNil, names)]
    unary con size names = do
      (pat, unused) <- go (size - 1) names
      return (con pat, unused)
    binary con size names = do
      (l, unused) <- go (size `div` 2) names
      (r, unused') <- go (size `div` 2) unused
      return (con l r, unused')
