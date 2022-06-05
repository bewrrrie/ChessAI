module ChessAI where

import           ChessLib (Board (..), CellCoords, Color, Game, Move, boardSize,
                           filterBoard, getCellCoordinates, getCellPiece,
                           getGameBoard, getMaybePieceColor, getMove,
                           isMoveAllowed)

rndDecide :: (Integer -> Integer) -> Integer -> Game -> Color -> Move
rndDecide rnd seed game moveColor = getMove srcCell destCell
  where
    pick seed (Board cells) = cells !! (seed `mod` length cells)
    board = getGameBoard game
    srcCell = pick rndSrcIdx ourPiecesSubBoard
    destCell =
      pick rndDestIdx $
      filterBoard (isMoveAllowed moveColor board . getMove srcCell) board
    rndSrcIdx = (`mod` boardSize board) $ fromIntegral seed
    rndDestIdx = (`mod` boardSize board) $ fromIntegral (rnd seed)
    destCoord = getCellCoordinates destCell
    ourPiecesSubBoard =
      filterBoard
        (\cell ->
           Just moveColor == (getMaybePieceColor . getCellPiece) cell &&
           0 < (boardSize . canMoveToSubBoard) cell)
        board
    canMoveToSubBoard cell =
      filterBoard (isMoveAllowed moveColor board . getMove cell) board
