{-# LANGUAGE DataKinds #-}

-- | Differential tests: the generic engine (with level-based and with naive
-- generalisation), the original, unoptimised engine
-- ('FreeFoilTypecheck.HindleyMilner.Inference') and the hand-specialised
-- engine ('FreeFoilTypecheck.HindleyMilner.SpecializedInference') must agree
-- on every program: either all of them reject it, or all of them infer the
-- same type scheme (up to renaming of variables).
module FreeFoilTypecheck.HindleyMilner.DifferentialSpec where

import Control.Monad (forM_, unless)
import Data.List (isSuffixOf, sort)
import FreeFoilTypecheck.GeneralTypecheck
  ( Generalization (..),
    HMType (..),
    UType,
    equivUpToRenaming,
    inferTypeSchemeClosed,
  )
import FreeFoilTypecheck.HindleyMilner.GeneralTypecheckSpec (openForAlls)
import qualified FreeFoilTypecheck.HindleyMilner.Inference as Original
import FreeFoilTypecheck.HindleyMilner.InferenceSpec (testFilesInDir)
import qualified FreeFoilTypecheck.HindleyMilner.Parser.Abs as Raw
import FreeFoilTypecheck.HindleyMilner.Parser.Par (myLexer, pExp)
import FreeFoilTypecheck.HindleyMilner.Parser.Print (printTree)
import FreeFoilTypecheck.HindleyMilner.Rules (fromTypeClosed, showHMType)
import qualified FreeFoilTypecheck.HindleyMilner.SpecializedInference as Specialized
import FreeFoilTypecheck.HindleyMilner.Syntax
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck

spec :: Spec
spec = do
  describe "the four engines agree on the test programs" $ do
    paths <- runIO (testFilesInDir "./test/FreeFoilTypecheck/HindleyMilner/files")
    forM_ (sort (filter (\p -> not (".expected.lam" `isSuffixOf` p)) paths)) $ \path -> it path $ do
      contents <- readFile path
      case pExp (myLexer contents) >>= toExpClosedChecked of
        Left _ -> return () -- parsing or scope error: not a question for type inference
        Right expr -> do
          let results = verdicts expr
          unless (allAgree results) $
            expectationFailure (unlines (map showVerdict results))

  describe "the four engines agree on random closed terms" $
    modifyMaxSuccess (const 3000) $ do
      prop "levels = naive = unoptimised = hand-specialised (all constructs)" $
        forAll (sized (genExp [])) agreeOn
      prop "levels = naive = unoptimised = hand-specialised (λ, application and let only)" $
        forAll (sized (genPureExp [])) agreeOn

agreeOn :: Raw.Exp -> Property
agreeOn raw =
  let results = verdicts (toExpClosed raw)
   in classify (any isAccepted results) "well-typed" $
        counterexample (printTree raw) $
          counterexample (unlines (map showVerdict results)) $
            allAgree results

-- | The verdict of one engine: an error, or a type scheme.
type Verdict = Either String (HMType (UType FoilTypePattern TypeSig))

-- | Verdicts of the four engines on a closed term.
verdicts :: Exp' -> [Verdict]
verdicts expr =
  [ inferTypeSchemeClosed LevelBased expr,
    inferTypeSchemeClosed Naive expr,
    fromForAlls <$> Original.inferTypeClosed expr,
    fromForAlls <$> Specialized.inferTypeClosed expr
  ]
  where
    -- a type with leading @forall@s, as the last two engines return it
    fromForAlls = MonoType . fromTypeClosed . openForAlls

isAccepted :: Verdict -> Bool
isAccepted = either (const False) (const True)

allAgree :: [Verdict] -> Bool
allAgree [] = True
allAgree (v : vs) = all (agree v) vs
  where
    agree (Left _) (Left _) = True
    agree (Right t1) (Right t2) = equivUpToRenaming t1 t2
    agree _ _ = False

showVerdict :: Verdict -> String
showVerdict = either ("error: " ++) showHMType

-- | Random closed terms of the pure fragment (variables, λ, application, let),
-- where well-typed terms are more frequent. Level adjustment matters for terms
-- such as @λx. let y = λz. x z in y@.
genPureExp :: [Raw.Ident] -> Int -> Gen Raw.Exp
genPureExp scope size
  | size <= 1 || null scope = if null scope then abstraction else variable
  | otherwise =
      frequency
        [ (2, variable),
          (3, abstraction),
          (4, Raw.EApp <$> genPureExp scope (size `div` 2) <*> genPureExp scope (size `div` 2)),
          (4, letExpression)
        ]
  where
    variable = Raw.EVar <$> elements scope
    name = elements (map Raw.Ident ["x", "y", "z", "f", "g"])
    abstraction = do
      x <- name
      Raw.EAbs (Raw.PatternVar x) . Raw.ScopedExp <$> genPureExp (x : scope) (size - 1)
    letExpression = do
      x <- name
      bound <- genPureExp scope (size `div` 2)
      body <- genPureExp (x : scope) (size `div` 2)
      return (Raw.ELet (Raw.PatternVar x) bound (Raw.ScopedExp body))

-- | Random closed terms of the HM language (without annotations).
-- Variables are drawn from a small pool, so that shadowing occurs.
genExp :: [Raw.Ident] -> Int -> Gen Raw.Exp
genExp scope size
  | size <= 1 = leaf
  | otherwise =
      frequency
        [ (2, leaf),
          (3, abstraction),
          (4, Raw.EApp <$> sub 2 <*> sub 2),
          (3, letExpression),
          (1, Raw.EIf <$> sub 3 <*> sub 3 <*> sub 3),
          (1, Raw.EAdd <$> sub 2 <*> sub 2),
          (1, Raw.EIsZero <$> sub 1),
          (1, forLoop)
        ]
  where
    sub k = genExp scope (size `div` k)
    leaf
      | null scope = constant
      | otherwise = frequency [(4, Raw.EVar <$> elements scope), (1, constant)]
    constant = elements [Raw.ETrue, Raw.EFalse, Raw.ENat 0, Raw.ENat 1]
    name = elements (map Raw.Ident ["x", "y", "z", "f", "g"])
    abstraction = do
      x <- name
      Raw.EAbs (Raw.PatternVar x) . Raw.ScopedExp <$> genExp (x : scope) (size - 1)
    letExpression = do
      x <- name
      bound <- genExp scope (size `div` 2)
      body <- genExp (x : scope) (size `div` 2)
      return (Raw.ELet (Raw.PatternVar x) bound (Raw.ScopedExp body))
    forLoop = do
      x <- name
      from <- sub 4
      to <- sub 4
      body <- genExp (x : scope) (size `div` 2)
      return (Raw.EFor (Raw.PatternVar x) from to (Raw.ScopedExp body))
