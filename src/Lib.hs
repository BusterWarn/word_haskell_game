module Lib
    ( outputGrid
    , formatGrid
    , findWord
    , findWordInLine
    , findWords
    , skew
    , diagonalize
    , coordsGrid
    , zipOverGrid
    , zipOverGridWith
    , gridWithCoords
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

data Cell = Cell (Integer, Integer) Char deriving (Eq, Ord, Show)
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

gridToLines :: Grid Char -> [String]
gridToLines grid =
  let horizontal = grid
      vertical = transpose grid
      diagonal_horizontal = diagonalize grid
      diagonal_vertical = diagonalize (map reverse horizontal)
      lines = horizontal ++ vertical ++ diagonal_horizontal ++ diagonal_vertical
  in lines ++ (map reverse lines)

findWord :: Grid Char -> String -> Maybe String
findWord grid word =
  let lines = gridToLines grid
      found = or $ map (findWordInLine word) lines
  in if found then Just word else Nothing

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

findWords :: Grid Char -> [String] -> [String]
findWords grid words =
  let foundWords = map (findWord grid) words
  in catMaybes foundWords

skew :: Grid Char -> Grid Char
skew [] = []
skew (l : ls) = do
  l : skew (map indent ls)
    where indent line = '_' : line

diagonalize :: Grid Char -> Grid Char
diagonalize = transpose . skew
