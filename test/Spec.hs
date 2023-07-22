import Test.Hspec
import Lib
import Data

testFindWord :: String -> Maybe String
testFindWord word =
  let result = findWord (gridWithCoords grid) word
  in case result of
    Nothing -> Nothing
    Just cells -> Just (map cellToChar cells)

main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "Should concatenate every line with a newline" $ do
      (formatGrid (gridWithCoords["abc", "def", "ghi"])) `shouldBe` "abc\ndef\nghi\n"
      
  describe "findWord" $ do
    it "Should find the words in the grid" $ do
      testFindWord "" `shouldBe` Just ""
      testFindWord "BASIC" `shouldBe` Just "BASIC"
      testFindWord "COBOL" `shouldBe` Just "COBOL"
      testFindWord "CSHARP" `shouldBe` Just "CSHARP"
      testFindWord "HASKELL" `shouldBe` Just "HASKELL"
      testFindWord "LISP" `shouldBe` Just "LISP"
      testFindWord "PERL" `shouldBe` Just "PERL"
      testFindWord "PHP" `shouldBe` Just "PHP"
      testFindWord "PYTHON" `shouldBe` Just "PYTHON"
      testFindWord "RUBY" `shouldBe` Just "RUBY"
      testFindWord "SCHEME" `shouldBe` Just "SCHEME"

    it "Should find the words spelled backwards in the grid" $ do
      testFindWord "CISAB" `shouldBe` Just "CISAB"
      testFindWord "LOBOC" `shouldBe` Just "LOBOC"
      testFindWord "PRAHSC" `shouldBe` Just "PRAHSC"
      testFindWord "LLEKSAH" `shouldBe` Just "LLEKSAH"
      testFindWord "PSIL" `shouldBe` Just "PSIL"
      testFindWord "LREP" `shouldBe` Just "LREP"
      testFindWord "NOHTYP" `shouldBe` Just "NOHTYP"
      testFindWord "YBUR" `shouldBe` Just "YBUR"
      testFindWord "EMEHCS" `shouldBe` Just "EMEHCS"
    
    it "should not find words that doesen't exist" $ do
      testFindWord "C++" `shouldBe` Nothing
      testFindWord "Spanish" `shouldBe` Nothing
      testFindWord "HaSkElL" `shouldBe` Nothing
      testFindWord "RUBBY" `shouldBe` Nothing
      testFindWord "snek" `shouldBe` Nothing

  describe "findWords" $ do
    it "Should find all the words in the grid" $ do
      let found = findWords (gridWithCoords grid) languages
          asStrings = map (map cellToChar) found
      asStrings `shouldBe` languages
    it "Should not find languages that does not exist" $ do
      let found = findWords (gridWithCoords grid) ["C++", "HASKELL", "ESPANJOL"]
          asStrings = map (map cellToChar) found
      asStrings `shouldBe` ["HASKELL"]

  describe "makeGame" $ do
    it "Should create a game with 0 score" $ do
      let game = makeGame grid languages
          gameScore = score game
      gameScore `shouldBe` 0

  describe "playGame" $ do
    it "Should calculate the correct score" $ do
      let game = makeGame grid languages
      let gameWithScore = playGame (playGame game "HASKELL") "PHP"
          newGameScore = score gameWithScore
      newGameScore `shouldBe` 2