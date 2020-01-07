module GameIO where

import System.IO
import Control.Monad
import Data.Maybe ( fromMaybe
                  , isNothing )

import ChessLib ( Game
                , Color (..)
                , Move
                , getMoveColor
                , getGameState
                , isGameFinished
                , initialGame
                , makeMove  )
import Parsing  ( parseMove
                , parseColor
                , parseQuit )
import ChessAI  ( aiDecide  )

-- | Random seed transformation function.
rnd :: Int -> Int
rnd seed = (a * seed + c) `mod` m
  where a = 32 * 8191 + 1
        c = 3921
        m = 64

getAIDecision :: Int -> Game -> Color -> Move
getAIDecision = aiDecide rnd

-- | Piece move inference function.
--   If it is our turn we parse the move from cmd
--   otherwise if it is AI's turn move must be inferred by minimax algorithm.
inferMove :: Int -> Game -> Maybe Color -> Maybe Color -> String -> Move
inferMove _     _    Nothing     _                      _   = error "Player color is Nothing!"
inferMove _     _    _           Nothing                _   = error "Move color is Nothing!"
inferMove seed  game playerColor moveColor@(Just color) cmd = if   moveColor == playerColor
                                                              then unpackParsedMove $ parseMove cmd
                                                              else getAIDecision seed game color
  where unpackParsedMove (Nothing,_,_,_)                 = (0,0,0,0)
        unpackParsedMove (_,Nothing,_,_)                 = (0,0,0,0)
        unpackParsedMove (_,_,Nothing,_)                 = (0,0,0,0)
        unpackParsedMove (_,_,_,Nothing)                 = (0,0,0,0)
        unpackParsedMove (Just x,Just y,Just x',Just y') = (x,y,x',y')

-- Main game loop.
gameLoop :: Int -> Maybe Color -> Game -> IO ()
gameLoop seed playerColor game = do { print game
                                    ; let maybeMoveColor = getMoveColor (getGameState game)
                                    ; if   maybeMoveColor == playerColor
                                      then putStr "Make your move: "
                                      else putStr "AI is going to make its decision."
                                    ; cmd <- getLine
                                    ; if   isNothing maybeMoveColor || parseQuit cmd
                                      then putStrLn "Finishing game process..."
                                      else do { let newSeed = rnd seed
                                              ; let move = inferMove newSeed game playerColor maybeMoveColor cmd
                                              ; let newGame = makeMove game move
                                              ; if   isGameFinished newGame
                                                then putStrLn "Game is finished.\nFinishing game process..."
                                                else gameLoop newSeed playerColor newGame } }

-- | IO function for reading input and getting output.
--   Main entry point to start the game.
playChess :: Int ->  IO ()
playChess seed = do { hSetBuffering stdout NoBuffering
                    ; putStr "Select color ('B'/'W' or 'q' to quit): "
                    ; cmd <- getLine
                    ; if   parseQuit cmd
                      then putStrLn "Finishing game process..."
                      else do { let playerColor = parseColor cmd
                              ; gameLoop   seed
                                         ( if   isNothing playerColor
                                           then Just White
                                           else playerColor )
                                           initialGame } }
