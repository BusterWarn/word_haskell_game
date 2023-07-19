import Test.Hspec
import Lib
import Data

main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "Should concatenate every line with a newline" $ do
      (formatGrid ["abc", "def", "ghi"]) `shouldBe` "abc\ndef\nghi\n"
      
  describe "findWord" $ do
    it "Should find the words in the grid" $ do
      findWord grid "" `shouldBe` Just ""
      findWord grid "BASIC" `shouldBe` Just "BASIC"
      findWord grid "COBOL" `shouldBe` Just "COBOL"
      findWord grid "CSHARP" `shouldBe` Just "CSHARP"
      findWord grid "HASKELL" `shouldBe` Just "HASKELL"
      findWord grid "LISP" `shouldBe` Just "LISP"
      findWord grid "PERL" `shouldBe` Just "PERL"
      findWord grid "PHP" `shouldBe` Just "PHP"
      findWord grid "PYTHON" `shouldBe` Just "PYTHON"
      findWord grid "RUBY" `shouldBe` Just "RUBY"
      findWord grid "SCHEME" `shouldBe` Just "SCHEME"

    it "Should find the words spelled backwards in the grid" $ do
      findWord grid "CISAB" `shouldBe` Just "CISAB"
      findWord grid "LOBOC" `shouldBe` Just "LOBOC"
      findWord grid "PRAHSC" `shouldBe` Just "PRAHSC"
      findWord grid "LLEKSAH" `shouldBe` Just "LLEKSAH"
      findWord grid "PSIL" `shouldBe` Just "PSIL"
      findWord grid "LREP" `shouldBe` Just "LREP"
      findWord grid "NOHTYP" `shouldBe` Just "NOHTYP"
      findWord grid "YBUR" `shouldBe` Just "YBUR"
      findWord grid "EMEHCS" `shouldBe` Just "EMEHCS"
    
    it "should not find words that doesen't exist" $ do
      findWord grid "C++" `shouldBe` Nothing
      findWord grid "Spanish" `shouldBe` Nothing
      findWord grid "HaSkElL" `shouldBe` Nothing
      findWord grid "RUBBY" `shouldBe` Nothing
      findWord grid "snek" `shouldBe` Nothing

  describe "findWords" $ do
    it "Should find all the words in the grid" $ do
      findWords grid languages `shouldBe` languages
    it "Should not find languages that does not exist" $ do
      findWords grid ["C++", "HASKELL", "ESPANJOL"] `shouldBe` ["HASKELL"]