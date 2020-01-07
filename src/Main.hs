module Main where

import System.Random

import GameIO

main :: IO ()
main = do { seed <- randomRIO (1,386)
          ; playChess seed }
