module Lib
    ( outputGrid
    , formatGrid
    , findWord
    , findWordInLine
    , findWordInCellLinePrefix
    , findWords
    , skew
    , diagonalize
    , coordsGrid
    , zipOverGrid
    , zipOverGridWith
    , gridWithCoords
    , Cell(Cell, Indent)
    , cellToChar
    , Game (gameGrid, gameWords)
    , makeGame
    , totalWords
    , score
    , playGame
    , formatGame
    , completed
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Map as M

data Game = Game
            {
              gameGrid :: Grid Cell,
              gameWords :: M.Map String (Maybe [Cell])
            }
            deriving Show

makeGame :: Grid Char -> [String] -> Game
makeGame grid words =
  let gwc = gridWithCoords grid
      tuplify word = (word, Nothing)
      list = map tuplify words
      dict = M.fromList list
  in Game gwc dict

totalWords :: Game -> Int
totalWords game = length . M.keys $ gameWords game

score :: Game -> Int
score game = length . catMaybes . M.elems $ gameWords game

completed :: Game -> Bool
completed game = score game == totalWords game

playGame :: Game -> String -> Game
playGame game word | not $ M.member word (gameWords game) = game
playGame game word =
  let grid = gameGrid game
      foundWord = findWord grid word
  in case foundWord of
      Nothing -> game
      Just cs ->
        let dict = gameWords game
            newDict = M.insert word foundWord dict
        in game { gameWords = newDict }

formatGame :: Game -> String
formatGame game@(Game grid _) =
     formatGrid grid
  ++ "\n\n"
  ++ (show $ score game)
  ++ " / "
  ++ (show $ totalWords game)

data Cell = Cell (Integer, Integer) Char 
          | Indent 
            deriving (Eq, Ord, Show)
type Grid a = [[a]]

zipOverGrid :: Grid a -> Grid b -> Grid (a, b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith =  zipWith . zipWith

-- Exercise: How does this work?
mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

coordsGrid :: Grid (Integer, Integer)
coordsGrid =
  let rows = map repeat [0..]
      cols = repeat [0..]
  in zipOverGrid rows cols

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords = zipOverGridWith Cell coordsGrid

outputGrid :: Grid Cell-> IO ()
outputGrid grid = do
  putStrLn $ "length: " ++ show (length (formatGrid grid))
  putStrLn $ formatGrid grid

formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOverGrid cellToChar

cellToChar :: Cell -> Char
cellToChar (Cell _ c) = c
cellToChar Indent = '?'

gridToLines :: Grid Cell -> [[Cell]]
gridToLines grid =
  let horizontal = grid
      vertical = transpose grid
      diagonal_horizontal = diagonalize grid
      diagonal_vertical = diagonalize (map reverse horizontal)
      lines = horizontal ++ vertical ++ diagonal_horizontal ++ diagonal_vertical
  in lines ++ (map reverse lines)

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
  let lines = gridToLines grid
      foundWords = map (findWordInLine word) lines
  in listToMaybe (catMaybes foundWords)

findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line =
  let found = findWordInCellLinePrefix [] word line
  in case found of
    Nothing -> findWordInLine word (tail line)
    cells@(Just _) -> cells

findWordInCellLinePrefix :: [Cell] -> String ->[Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (char : chars) (cell : cells) | char == (cellToChar cell)
  = findWordInCellLinePrefix (cell : acc) chars cells
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing

findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words =
  let foundWords = map (findWord grid) words
  in catMaybes foundWords

skew :: Grid Cell -> Grid Cell
skew [] = []
skew (l : ls) = do
  l : skew (map indent ls)
    where indent line = Indent : line

diagonalize :: Grid Cell -> Grid Cell
diagonalize = transpose . skew
