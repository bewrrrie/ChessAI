{-# LANGUAGE BlockArguments #-}
module GameIO where

import ChessLib
import Parsing
import System.IO
import Control.Monad

gameLoop :: Game -> IO ()
gameLoop game = do { print game
                   ; cmd <- getLine
                   ; if parseQuit cmd
                     then putStrLn "Finishing game process..."
                     else do { let move = parseMove cmd
                             ; let newGame = makeMove game move
                             ; if isGameFinished newGame
                               then putStrLn "Finishing game process..."
                               else gameLoop newGame } }

-- IO function for reading input and getting output
playChess :: IO ()
playChess = do { hSetBuffering stdout NoBuffering
               ; putStr "Select color ('B'/'W' or 'q' to quit): "
               ; cmd <- getLine
               ; if parseQuit cmd
                 then putStrLn "Finishing game process..."
                 else do {
                         ; let playerColor = parseColor cmd
                         ; gameLoop initialGame } }
