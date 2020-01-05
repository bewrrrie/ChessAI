module GameIO where

import ChessLib

-- Play game function
playChess :: IO ()
playChess = play initialGame where
  play _ = putStrLn "LOL"
