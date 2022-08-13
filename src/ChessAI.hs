module ChessAI where

import           ChessLib (Board (..), Color, Game, Move, boardSize,
                           filterBoard, getCellPiece, getGameBoard,
                           getMaybePieceColor, getMove, isMoveAllowed)

decide :: (Integer -> Integer) -> Integer -> Game -> Color -> Move
decide rnd seed game moveColor = getMove srcCell destCell
  where
    pick n (Board cells) = cells !! (n `mod` length cells)
    board = getGameBoard game
    srcCell = pick rndSrcIdx ourPiecesSubBoard
    destCell =
      pick rndDestIdx $
      filterBoard (isMoveAllowed moveColor board . getMove srcCell) board
    rndSrcIdx = (`mod` boardSize board) $ fromIntegral seed
    rndDestIdx = (`mod` boardSize board) $ fromIntegral (rnd seed)
    ourPiecesSubBoard =
      filterBoard
        (\cell ->
           Just moveColor == (getMaybePieceColor . getCellPiece) cell &&
           0 < (boardSize . canMoveToSubBoard) cell)
        board
    canMoveToSubBoard cell =
      filterBoard (isMoveAllowed moveColor board . getMove cell) board
