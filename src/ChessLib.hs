module ChessLib where

import Data.Maybe(isNothing)

-- Chess piece type.
data Color = White | Black deriving (Eq, Show)
data PieceType = Pawn
  | Rook   | Knight
  | Bishop | Queen
  | King deriving (Eq, Show)
data Piece = Piece Color PieceType deriving (Eq)

instance Show Piece where
--Show white pieces:
  show (Piece White Pawn)   = "♙"
  show (Piece White Rook)   = "♖"
  show (Piece White Knight) = "♘"
  show (Piece White Bishop) = "♗"
  show (Piece White Queen)  = "♕"
  show (Piece White King)   = "♔"
--Show black pieces:
  show (Piece Black Pawn)   = "♟"
  show (Piece Black Rook)   = "♜"
  show (Piece Black Knight) = "♞"
  show (Piece Black Bishop) = "♝"
  show (Piece Black Queen)  = "♛"
  show (Piece Black King)   = "♚"

-- Game board cell type.
data Cell = Cell Color (Maybe Piece) deriving (Eq)

instance Show Cell where
  show (Cell _ (Just piece)) = show piece
  show (Cell White Nothing)  = "▫"
  show (Cell Black Nothing)  = "▪"

-- Game board type.
newtype Board = Board [[Cell]] deriving (Eq)

-- String representation of game board.
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

-- Chess game state type.
data State = Move Color | Check Color
           | Mate Color | Draw
           | CheckMate Color deriving (Eq)

instance Show State where
  show (Move White)      = "Move for White."
  show (Move Black)      = "Move for Black."
  show  Draw             = "Draw!"
  show (Check White)     = "White is in check."
  show (Check Black)     = "Black is in check."
  show (CheckMate White) = "White is in checkmate."
  show (CheckMate Black) = "Black is in checkmate."

data Game = Game Board State deriving (Eq)

instance Show Game where
  show (Game board state) = show board ++ "\n" ++ show state

-- Black pieces.
blackPawn, blackRook, blackKnight, blackBishop, blackQueen, blackKing :: Piece
blackPawn   = Piece Black Pawn
blackRook   = Piece Black Rook
blackKnight = Piece Black Knight
blackBishop = Piece Black Bishop
blackQueen  = Piece Black Queen
blackKing   = Piece Black King

-- White pieces.
whitePawn, whiteRook, whiteKnight, whiteBishop, whiteQueen, whiteKing :: Piece
whitePawn   = Piece White Pawn
whiteRook   = Piece White Rook
whiteKnight = Piece White Knight
whiteBishop = Piece White Bishop
whiteQueen  = Piece White Queen
whiteKing   = Piece White King

-- Initial game board state.
initialBoard :: Board
initialBoard = Board cellsList
  where boardSize = 8
        cellsList = [ [placePiece i j | i <- [0..boardSize - 1]]
                                      | j <- [0..boardSize - 1] ]
        placePiece x y                   = Cell (color x y) (maybePiece x y)
        color x y | (x + y) `mod` 2 == 0 = White
                  | otherwise            = Black

        maybePiece 7 0 = Just blackRook
        maybePiece 0 0 = Just blackRook
        maybePiece 1 0 = Just blackKnight
        maybePiece 6 0 = Just blackKnight
        maybePiece 2 0 = Just blackBishop
        maybePiece 5 0 = Just blackBishop
        maybePiece 3 0 = Just blackQueen
        maybePiece 4 0 = Just blackKing
        maybePiece _ 1 = Just blackPawn

        maybePiece _ 6 = Just whitePawn
        maybePiece 4 7 = Just whiteKing
        maybePiece 3 7 = Just whiteQueen
        maybePiece 5 7 = Just whiteBishop
        maybePiece 2 7 = Just whiteBishop
        maybePiece 6 7 = Just whiteKnight
        maybePiece 1 7 = Just whiteKnight
        maybePiece 0 7 = Just whiteRook
        maybePiece 7 7 = Just whiteRook

        maybePiece _ _ = Nothing

-- Initial game state.
initialGame = Game initialBoard (Move White)

-- Piece functions.
getMaybePieceColor :: Maybe Piece -> Maybe Color
getMaybePieceColor (Just (Piece color _)) = Just color
getMaybePieceColor Nothing = Nothing

-- Cell functions.
emptyCell :: Color -> Cell
emptyCell color = Cell color Nothing

getCellColor :: Cell -> Color
getCellColor (Cell color _) = color

setCellColor :: Color -> Cell -> Cell
setCellColor color (Cell _ mbp) = Cell color mbp

getCellPiece :: Cell -> Maybe Piece
getCellPiece (Cell _ mbp) = mbp

-- State functions
getMoveColor :: State -> Maybe Color
getMoveColor (Move color) = Just color
getMoveColor _            = Nothing

-- Switch color to opposite function
switchColor :: Color -> Color
switchColor Black = White
switchColor White = Black

switchMaybeColor :: Maybe Color -> Maybe Color
switchMaybeColor (Just Black) = Just White
switchMaybeColor (Just White) = Just Black
switchMaybeColor Nothing = Nothing

-- Game board functions.
getCell :: Board -> (Int, Int) -> Cell
getCell (Board cells) (x,y) = cells !! y !! x

setCell :: (Int, Int) -> Cell -> Board -> Board
setCell (x,y) cell (Board rows) = Board (replace y newRow rows)
  -- Replace list element function.
  where replace :: Int -> a -> [a] -> [a]
        replace n newElement xs = take n xs ++ [newElement] ++ drop (n + 1) xs
        newRow = replace x cell (rows !! y)

isNothingCell :: Cell -> Bool
isNothingCell (Cell _ mbp) = isNothing mbp

-- Game type functions.
getGameState :: Game -> State
getGameState (Game _ state) = state

isGameFinished :: Game -> Bool
isGameFinished (Game _ state) = state `elem` [ Draw
                                             , CheckMate White
                                             , CheckMate Black ]

-- Transform game state function.
isMoveAllowed :: Cell -> Cell -> (Int, Int, Int, Int) -> Bool
isMoveAllowed (Cell _ _) destCell move@(x,y,x',y') = False --TODO restrict moves for specific pieces


transformGame :: Game -> (Int, Int, Int, Int) -> Game
transformGame game@(Game board state) move@(x,y,x',y') = Game (transformBoard board state)
                                                              (transformState state)
  where transformState (Move color) = if Just color == maybePieceColorOnSrcCell
                                      then Move (switchColor color)
                                      else Move color
        transformState state        = state

        maybePieceColorOnSrcCell = getMaybePieceColor (getCellPiece srcCell)
        srcCell                  = getCell board (x,  y )
        destCell                 = getCell board (x', y')

        -- TODO checks, checkmates and draws
        transformBoard board (Move color) = if ( isNothingCell srcCell ||
                                                 Just color /= maybePieceColorOnSrcCell )
                                               && isMoveAllowed srcCell destCell move
                                            then board
                                            else setCell (x', y') newDestCell $
                                                 setCell (x,  y ) newSrcCell board
                                            where newSrcCell  = emptyCell (getCellColor srcCell)
                                                  newDestCell = Cell (getCellColor destCell) (getCellPiece srcCell)
        transformBoard board _            = board

makeMove :: Game -> (Int, Int, Int, Int) -> Game
makeMove game@(Game board (CheckMate White)) _ = game
makeMove game@(Game board (CheckMate Black)) _ = game
makeMove game@(Game board Draw)              _ = game
makeMove game@(Game _     (Move _))       move = transformGame game move
