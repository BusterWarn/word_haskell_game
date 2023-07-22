module Main (main) where

import Lib
import Data
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let game = makeGame grid languages
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
