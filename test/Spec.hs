import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      "someString" `shouldBe` "someString"