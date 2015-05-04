import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main = hspec $ do
  describe "This" $ do
    it "should word" $ do
      head [23..] `shouldBe` (23::Int)

    it "gen" $ do
      property $ \x xs -> head (x:xs) == (x::Int)

    it "throws" $ do
      evaluate (head []) `shouldThrow` anyException

