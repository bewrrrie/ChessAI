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
inferMove :: Game -> Maybe Color -> Maybe Color -> String -> (Int, Int, Int, Int)
inferMove _    Nothing     _                      _   = error "Move color is Nothing!"
inferMove _    _           Nothing                _   = error "Move color is Nothing!"
inferMove game playerColor moveColor@(Just color) cmd = if moveColor == playerColor
                                                        then unpackParsedMove $ parseMove cmd
                                                        else aiDecide game color
  where unpackParsedMove (Nothing,_,_,_)                 = (0,0,0,0)
        unpackParsedMove (_,Nothing,_,_)                 = (0,0,0,0)
        unpackParsedMove (_,_,Nothing,_)                 = (0,0,0,0)
        unpackParsedMove (_,_,_,Nothing)                 = (0,0,0,0)
        unpackParsedMove (Just x,Just y,Just x',Just y') = (x,y,x',y')

-- Main game loop.
gameLoop :: Maybe Color -> Game -> IO ()
gameLoop playerColor game = do { print game
                               ; let maybeMoveColor = getMoveColor (getGameState game)
                               ; if maybeMoveColor == playerColor
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
                         ; let playerColor = parseColor cmd
                         ; gameLoop playerColor initialGame } }
