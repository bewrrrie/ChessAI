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

aiDecide :: Game -> Color -> Move
aiDecide game moveColor = getMove srcCell destCell
  where pick seed (Board cells) = cells !! (seed `mod` length cells)
        board     = getGameBoard game
        srcCell   = pick rndIdx ourPiecesSubBoard
        destCell  = pick rndIdx $ filterBoard ( isMoveAllowed moveColor board .
                                                getMove srcCell ) board
        rndIdx    = 0
        destCoord = getCellCoordinates destCell
        ourPiecesSubBoard = filterBoard ( \cell -> Just moveColor == (getMaybePieceColor . getCellPiece) cell
                                                && 0 < (boardSize . canMoveToSubBoard) cell ) board
        canMoveToSubBoard cell = filterBoard ( isMoveAllowed moveColor board .
                                               getMove cell ) board
