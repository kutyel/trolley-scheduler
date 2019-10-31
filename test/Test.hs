module Test where

import qualified Lib as L
import Test.Hspec

main = hspec $ do
  describe "something" $ do
    it "goes" $ do
      'a' `shouldBe` 'a'
