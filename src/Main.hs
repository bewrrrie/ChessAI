module Main where

import Text.Printf
import System.Random

import GameIO

-- | Entry point function.
main :: IO ()
main = do { seed <- randomRIO (0, 10^40)
          ; printf "Current game seed: %d\n" seed
          ; playChess seed }
