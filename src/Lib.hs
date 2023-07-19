module Lib
    ( outputGrid
    , formatGrid
    , findWord
    , findWordInLine
    , findWords
    , skew
    , diagonalize
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

type Grid = [String]

outputGrid :: Grid -> IO ()
outputGrid grid = do
  putStrLn $ "length: " ++ show (length (formatGrid grid))
  putStrLn $ formatGrid grid

formatGrid :: Grid -> String
formatGrid = unlines

gridToLines :: Grid -> [String]
gridToLines grid =
  let horizontal = grid
      vertical = transpose grid
      diagonal_horizontal = diagonalize grid
      diagonal_vertical = diagonalize (map reverse horizontal)
      lines = horizontal ++ vertical ++ diagonal_horizontal ++ diagonal_vertical
  in lines ++ (map reverse lines)

findWord :: Grid -> String -> Maybe String
findWord grid word =
  let lines = gridToLines grid
      found = or $ map (findWordInLine word) lines
  in if found then Just word else Nothing

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

findWords :: Grid -> [String] -> [String]
findWords grid words =
  let foundWords = map (findWord grid) words
  in catMaybes foundWords

skew :: Grid -> Grid
skew [] = []
skew (l : ls) = do
  l : skew (map indent ls)
    where indent line = '_' : line

diagonalize :: Grid -> Grid
diagonalize = transpose . skew
