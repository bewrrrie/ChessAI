{-# LANGUAGE BlockArguments #-}
module GameIO where

import ChessLib
import Parsing
import System.IO
import Control.Monad
import Data.Maybe (fromMaybe)

-- Main game loop.
gameLoop :: Color -> Color -> Game -> IO ()
gameLoop plColor curColor game = do { print game
                                    ; cmd <- getLine
                                    ; if parseQuit cmd
                                      then putStrLn "Finishing game process..."
                                      else do { let move = parseMove cmd
                                              ; let newGame = makeMove game move
                                              ; let newColor = switchColor curColor
                                              ; if isGameFinished newGame
                                                then putStrLn "Finishing game process..."
                                                else gameLoop plColor curColor newGame } }

-- IO function for reading input and getting output.
-- Main entry point to start the game.
playChess :: IO ()
playChess = do { hSetBuffering stdout NoBuffering
               ; putStr "Select color ('B'/'W' or 'q' to quit): "
               ; cmd <- getLine
               ; if parseQuit cmd
                 then putStrLn "Finishing game process..."
                 else do {
                         ; let playerColor = fromMaybe White (parseColor cmd)
                         ; gameLoop playerColor White initialGame } }
