import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "add" $ do
    it "should add two numbers correctly" $ do
      add 2 3 `shouldBe` 5
    it "should handle negative numbers" $ do
      add (-2) 3 `shouldBe` 1
    it "should handle zero" $ do
      add 0 5 `shouldBe` 5
