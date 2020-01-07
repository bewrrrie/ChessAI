module Main where

import System.Random

import GameIO

-- | Entry point function.
main :: IO ()
main = do { seed <- randomRIO (0,63)
          ; playChess seed }
