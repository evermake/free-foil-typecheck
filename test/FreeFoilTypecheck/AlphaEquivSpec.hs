{-# LANGUAGE OverloadedStrings #-}

module FreeFoilTypecheck.AlphaEquivSpec where

import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Free.Foil as FreeFoil
import qualified FreeFoilTypecheck.HindleyMilner.Syntax as HM
import qualified FreeFoilTypecheck.SystemF.Syntax.Term as SystemF
import Test.Hspec

spec :: Spec
spec = do
  describe "Hindley-Milner expressions" $ do
    it "identifies terms up to renaming of bound variables" $
      hmExp "λx. x + 1" "λy. y + 1" `shouldBe` True
    it "distinguishes literals" $
      hmExp "1" "2" `shouldBe` False
    it "distinguishes type annotations" $
      hmExp "λx. x : Nat -> Nat" "λx. x : Bool -> Bool" `shouldBe` False

  describe "Hindley-Milner types" $ do
    it "identifies types up to renaming of bound variables" $
      ("forall a. a -> ?u" :: HM.Type') `shouldBe` "forall b. b -> ?u"
    it "distinguishes unification variables" $
      ("?a -> ?a" :: HM.Type') `shouldNotBe` "?a -> ?b"

  describe "System F terms" $ do
    it "identifies terms up to renaming of bound variables" $
      systemF "Λa. λx : a. x" "Λb. λy : b. y" `shouldBe` True
    it "distinguishes literals" $
      systemF "1" "2" `shouldBe` False
    it "distinguishes unification variables" $
      systemF "?a -> ?a" "?a -> ?b" `shouldBe` False
  where
    hmExp :: HM.Exp' -> HM.Exp' -> Bool
    hmExp = FreeFoil.alphaEquiv Foil.emptyScope

    systemF :: SystemF.Term' -> SystemF.Term' -> Bool
    systemF (SystemF.Term l) (SystemF.Term r) = FreeFoil.alphaEquiv Foil.emptyScope l r
