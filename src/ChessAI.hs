module ChessAI where

import ChessLib ( Game
                , Board (..)
                , Color
                , Move
                , CellCoords
                , getCellCoordinates
                , getCellPiece
                , getMaybePieceColor
                , getGameBoard
                , filterBoard
                , getMove
                , boardSize
                , isMoveAllowed )

aiDecide :: (Int -> Int) -> Int -> Game -> Color -> Move
aiDecide rnd seed game moveColor = getMove srcCell destCell
  where pick seed (Board cells) = cells !! (seed `mod` length cells)
        board      = getGameBoard game
        srcCell    = pick rndSrcIdx ourPiecesSubBoard
        destCell   = pick rndDestIdx $ filterBoard ( isMoveAllowed moveColor board .
                                                     getMove srcCell ) board
        rndSrcIdx  = seed
        rndDestIdx = rnd seed
        destCoord  = getCellCoordinates destCell
        ourPiecesSubBoard = filterBoard ( \cell -> Just moveColor == (getMaybePieceColor . getCellPiece) cell
                                                && 0 < (boardSize . canMoveToSubBoard) cell ) board
        canMoveToSubBoard cell = filterBoard ( isMoveAllowed moveColor board .
                                               getMove cell ) board
