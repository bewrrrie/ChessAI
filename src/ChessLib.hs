module ChessLib where

import Data.Maybe (isNothing)

-- Chess piece type
data Color = White | Black deriving (Eq, Show)
data PieceType = Pawn
  | Rook   | Knight
  | Bishop | Queen
  | King deriving (Eq, Show)
data Piece = Piece Color PieceType deriving (Eq)
instance Show Piece where
--Show white pieces
  show (Piece White Pawn)   = "♙"
  show (Piece White Rook)   = "♖"
  show (Piece White Knight) = "♘"
  show (Piece White Bishop) = "♗"
  show (Piece White Queen)  = "♕"
  show (Piece White King)   = "♔"
--Show black pieces
  show (Piece Black Pawn)   = "♟"
  show (Piece Black Rook)   = "♜"
  show (Piece Black Knight) = "♞"
  show (Piece Black Bishop) = "♝"
  show (Piece Black Queen)  = "♛"
  show (Piece Black King)   = "♚"

-- Game board cell type
data Cell = Cell Color (Maybe Piece) deriving (Eq)
instance Show Cell where
  show (Cell _ (Just piece)) = show piece
  show (Cell White Nothing)  = "▫"
  show (Cell Black Nothing)  = "▪"

-- Game board type
newtype Board = Board [[Cell]] deriving (Eq)

-- String representation of game board
instance Show Board where
  show (Board rows) = "\n" ++ showLetterCoordinates rows ++
                      showUpperBorder rows ++ showRows rows ++ showLowerBorder rows
                      ++ showLetterCoordinates rows
    where showUpperBorder (row:rows) = " ┏" ++ concat (replicate (length row - 1) "━┯") ++ "━┓\n"
          showLowerBorder (row:rows) = " ┗" ++ concat (replicate (length row - 1) "━┷") ++ "━┛\n"
          showRowsSplitter row       = " ┠" ++ concat (replicate (length row - 1) "─┼") ++ "─┨\n"
          showRows   [row]           = "1┃" ++ showCellsOnCurrentRow row ++ "┃1\n"
          showRows   (row:rows)      = coordinate ++ "┃" ++ showCellsOnCurrentRow row ++ "┃" ++ coordinate
                                       ++ "\n" ++ showRowsSplitter row
                                       ++ showRows rows where coordinate = show (1 + length rows)
          showCellsOnCurrentRow [cell]       = show cell
          showCellsOnCurrentRow (cell:cells) = show cell ++ "│" ++ showCellsOnCurrentRow cells
          showLetterCoordinates (row:rows)   = " " ++ concatMap (\ x -> " " ++ [x]) ['A' .. 'H'] ++ "\n"

-- Chess game state type
data State = Move  Color | Draw Color
           | Check Color | Mate Color
           | CheckMate Color deriving (Eq, Show)
data Game = Game { gameBoard :: Board
                 , gameState :: State
                 } deriving (Eq, Show)

-- Black pieces
blackPawn, blackRook, blackKnight, blackBishop, blackQueen, blackKing :: Piece
blackPawn   = Piece Black Pawn
blackRook   = Piece Black Rook
blackKnight = Piece Black Knight
blackBishop = Piece Black Bishop
blackQueen  = Piece Black Queen
blackKing   = Piece Black King

-- White pieces
whitePawn, whiteRook, whiteKnight, whiteBishop, whiteQueen, whiteKing :: Piece
whitePawn   = Piece White Pawn
whiteRook   = Piece White Rook
whiteKnight = Piece White Knight
whiteBishop = Piece White Bishop
whiteQueen  = Piece White Queen
whiteKing   = Piece White King

-- Initial game board state
initialBoard :: Board
initialBoard = Board cellsList
  where boardSize = 8
        cellsList = [ [placePiece i j | i <- [1..boardSize]]
                                      | j <- [1..boardSize] ]
        placePiece x y                   = Cell (color x y) (maybePiece x y)
        color x y | (x + y) `mod` 2 == 0 = White
                  | otherwise            = Black

        maybePiece 8 8 = Just blackRook
        maybePiece 1 8 = Just blackRook
        maybePiece 2 8 = Just blackKnight
        maybePiece 7 8 = Just blackKnight
        maybePiece 3 8 = Just blackBishop
        maybePiece 6 8 = Just blackBishop
        maybePiece 4 8 = Just blackQueen
        maybePiece 5 8 = Just blackKing
        maybePiece _ 7 = Just blackPawn

        maybePiece _ 2 = Just whitePawn
        maybePiece 5 1 = Just whiteKing
        maybePiece 4 1 = Just whiteQueen
        maybePiece 6 1 = Just whiteBishop
        maybePiece 3 1 = Just whiteBishop
        maybePiece 7 1 = Just whiteKnight
        maybePiece 2 1 = Just whiteKnight
        maybePiece 1 1 = Just whiteRook
        maybePiece 8 1 = Just whiteRook

        maybePiece _ _ = Nothing

-- Initial game state
initialGame = Game { gameBoard = initialBoard
                   , gameState = Move White }
