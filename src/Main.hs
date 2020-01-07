module Main where

import GameIO

-- | Initial random seed.
rndSeed :: Int
rndSeed = 17

main :: IO ()
main = playChess rndSeed
