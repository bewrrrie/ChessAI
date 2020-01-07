module Main where

import System.Random

import GameIO

main :: IO ()
main = do { rnd <- randomRIO (1, 524287)
          ; playChess rnd }
