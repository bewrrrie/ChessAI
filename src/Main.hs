module Main where

import           System.Random
import           Text.Printf

import           GameIO

-- | Entry point function.
main :: IO ()
main = do
  seed <- randomRIO (0, 10 ^ 40)
  printf "Current game seed: %d\n" seed
  playChess seed
