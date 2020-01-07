module Main where

import System.Random

import GameIO

main :: IO ()
main = do { seed <- randomRIO (0,63)
          ; playChess seed }
