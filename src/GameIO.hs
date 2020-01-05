{-# LANGUAGE BlockArguments #-}
module GameIO where

import ChessLib
import Parsing
import System.IO
import Control.Monad

gameLoop :: Game -> IO ()
gameLoop game = do { print game
                   ; moveStr <- getLine
                   ; let move = parseMove moveStr
                   ; let newGame = makeMove game move
                   ; if isGameFinished newGame
                     then putStrLn "Finishing game process..." 
                     else gameLoop newGame }

-- IO function for reading input and getting output
playChess :: IO ()
playChess = do { hSetBuffering stdout NoBuffering
               ; putStr "Select color (B/W): "
               ; colorStr <- getLine
               ; let playerColor = parseColor colorStr
               ; gameLoop initialGame }
