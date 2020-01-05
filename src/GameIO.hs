module GameIO where

import Game

-- Play game function
playChess :: IO ()
playChess = play initialGame where
  play _ = putStrLn "LOL"
