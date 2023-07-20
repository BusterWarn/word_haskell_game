module Main (main) where

import Lib
import Data

main :: IO ()
main =
  let gridOfCells = gridWithCoords grid
  in outputGrid gridOfCells
