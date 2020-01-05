{-# LANGUAGE BlockArguments #-}
module GameIO where

import ChessLib
import ChessAI
import Parsing
import System.IO
import Control.Monad
import Data.Maybe (fromMaybe, isNothing)

-- Piece move inference function.
-- If it is our turn we parse the move from cmd
-- otherwise if it is AI's turn move must be inferred by minimax algorithm.
inferMove :: Game -> Color -> Maybe Color -> String -> (Int, Int, Int, Int)
inferMove game playerColor (Just color) cmd = if color == playerColor
                                              then parseMove cmd
                                              else aiDecide game color
inferMove _    _            Nothing     _   = error "Move color is Nothing!"

-- Main game loop.
gameLoop :: Color -> Game -> IO ()
gameLoop playerColor game = do { print game
                               ; let maybeMoveColor = getMoveColor (getGameState game)
                               ; if maybeMoveColor == Just playerColor
                                 then putStr "Make your move: "
                                 else putStr "AI is going to make its decision."
                               ; cmd <- getLine
                               ; if isNothing maybeMoveColor
                                 then putStrLn "Game is finished.\nFinishing game process..."
                                 else do { let move = inferMove game playerColor maybeMoveColor cmd
                                         ; let newGame = makeMove game move
                                         ; if isGameFinished newGame
                                           then putStrLn "Game is finished.\nFinishing game process..."
                                           else gameLoop playerColor newGame } }

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
                         ; gameLoop playerColor initialGame } }
