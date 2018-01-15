module Main where

import Graphics.Gloss

import Lib
import Examples

main :: IO ()
main =
  display (InWindow "L-System" (200, 200) (10, 10)) black (color white pic)
  where pic = drawLSystem $ iterateLSystem 5 gosper
