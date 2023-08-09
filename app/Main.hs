module Main (main) where

import Lib
import Data
import System.IO
import System.Random (newStdGen)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  gen <- newStdGen
  let filledInGrid = fillInBlanks gen grid
      game = makeGame filledInGrid languages
  playTurn game

playTurn game = do
  putStrLn . formatGame $ game
  putStr "Please enter a word> "
  word <- getLine
  let newGame = playGame game word
  putStrLn . formatGame $ newGame
  if completed newGame then
    putStrLn "Woohoo!"
  else
    playTurn newGame
