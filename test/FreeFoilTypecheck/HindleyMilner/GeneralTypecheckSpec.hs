{-# LANGUAGE DataKinds #-}

-- | Runs the generic engine ('FreeFoilTypecheck.GeneralTypecheck')
-- on the same test programs as 'FreeFoilTypecheck.HindleyMilner.InferenceSpec'.
module FreeFoilTypecheck.HindleyMilner.GeneralTypecheckSpec where

import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Free.Foil as FreeFoil
import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.List (isSuffixOf, sort)
import FreeFoilTypecheck.GeneralTypecheck
  ( Generalization (..),
    HMType (..),
    TypeCheck (..),
    TypingContext (..),
    UType,
    canonicalHMType,
    emptyTypingContext,
    equivUpToRenaming,
    inferTypeNewClosed,
    inferTypeSchemeClosed,
  )
import FreeFoilTypecheck.HindleyMilner.InferenceSpec (testFilesInDir)
import qualified FreeFoilTypecheck.HindleyMilner.Parser.Abs as Raw
import FreeFoilTypecheck.HindleyMilner.Parser.Par (myLexer, pExp, pType)
import FreeFoilTypecheck.HindleyMilner.Rules (fromTypeClosed, showHMType)
import FreeFoilTypecheck.HindleyMilner.Syntax
import FreeFoilTypecheck.MetaVar (MetaVar (..), sizeMetaVarMap)
import System.FilePath (replaceExtension)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  forM_ [LevelBased, Naive] $ \generalization -> do
    describe ("well-typed expressions (generic engine, " ++ show generalization ++ ")") $ do
      paths <- runIO (testFilesInDir "./test/FreeFoilTypecheck/HindleyMilner/files/well-typed")
      forM_ (sort (filter (\p -> not (".expected.lam" `isSuffixOf` p)) paths)) $ \path -> it path $ do
        contents <- readFile path
        expectedTypeContents <- readFile (replaceExtension path ".expected.lam")
        genericTypeMatches generalization contents expectedTypeContents `shouldBe` Right True

    describe ("ill-typed expressions (generic engine, " ++ show generalization ++ ")") $ do
      paths <- runIO (testFilesInDir "./test/FreeFoilTypecheck/HindleyMilner/files/ill-typed")
      forM_ (sort paths) $ \path -> it path $ do
        contents <- readFile path
        genericRejects generalization contents `shouldBe` Right True

  describe "substitution (generic engine)" $
    forM_ [1 .. 8 :: Int] $ \n ->
      it ("binds each unification variable at most once after " ++ show n ++ " nested lets") $ do
        let expr = toExpClosed (either error id (pExp (myLexer (nestedLets n))))
        case runTypeCheck (inferHM expr) emptyTypingContext of
          Left err -> expectationFailure err
          Right (_type, ctx) -> do
            let MetaVar created = tcFreshId ctx
            sizeMetaVarMap (tcSubst ctx) `shouldSatisfy` (<= created)

inferHM :: Exp' -> TypeCheck (UType FoilTypePattern TypeSig) Foil.VoidS (UType FoilTypePattern TypeSig Foil.VoidS)
inferHM = inferTypeNewClosed

inferScheme :: Generalization -> Exp' -> Either String (HMType (UType FoilTypePattern TypeSig))
inferScheme = inferTypeSchemeClosed

-- | @λf0. let f1 = λx. f0 x in … let fn = λx. f(n-1) x in fn@
nestedLets :: Int -> String
nestedLets n =
  "λf0. "
    ++ concat ["let f" ++ show i ++ " = λx. f" ++ show (i - 1) ++ " x in " | i <- [1 .. n]]
    ++ "f"
    ++ show n

-- | Whether the generic engine rejects a program (with a scope or type error).
-- A parsing error is reported as 'Left', since test programs must parse.
genericRejects :: Generalization -> String -> Either String Bool
genericRejects generalization source = do
  raw <- pExp (myLexer source)
  case toExpClosedChecked raw of
    Left _scopeError -> Right True
    Right expr -> Right (isLeft (inferScheme generalization expr))

-- | Whether the generic engine infers the expected type (up to renaming).
genericTypeMatches :: Generalization -> String -> String -> Either String Bool
genericTypeMatches generalization source expectedSource = do
  expr <- pExp (myLexer source) >>= toExpClosedChecked
  expected <- fromTypeClosed . openForAlls . toTypeClosed <$> pType (myLexer expectedSource)
  actualScheme <- inferScheme generalization expr
  let actual = canonicalHMType actualScheme
  if equivUpToRenaming actual (MonoType expected)
    then Right True
    else
      Left $
        unlines
          [ "types do not match",
            "expected:",
            show expected,
            "but actual is:",
            showHMType actual
          ]

-- | Replace the leading @forall@s of an expected type with unification variables.
openForAlls :: Type' -> Type'
openForAlls = go (0 :: Int)
  where
    go i (TForAll (FoilTPatternVar binder) body) =
      let x = TUVar (Raw.UVarIdent ("?forall" ++ show i))
          subst = Foil.addSubst Foil.identitySubst binder x
       in go (i + 1) (FreeFoil.substitute Foil.emptyScope subst body)
    go _ t = t
