module ChessLib where

import Data.Maybe ( isNothing
                  , fromMaybe
                  , isJust )

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
type CellCoords = (Int,Int)
type Move = (Int,Int,Int,Int)
data Cell = Cell CellCoords Color (Maybe Piece) deriving (Eq)

instance Show Cell where
  show (Cell _ _ (Just piece)) = show piece
  show (Cell _ White Nothing)  = "▫"
  show (Cell _ Black Nothing)  = "▪"

-- Game board type.
newtype Board = Board [Cell] deriving (Eq)

-- String representation of game board.
instance Show Board where
  show (Board cells) = "\n" ++ showLetterCoordinates
                            ++ showUpperBorder
                            ++ showCells cells 8
                            ++ showLowerBorder
                            ++ showLetterCoordinates
    where showLetterCoordinates = " "  ++ concatMap (\ x -> " " ++ [x]) ['A' .. 'H'] ++ "\n"
          showUpperBorder       = " ┏" ++ concat    (replicate 7 "━┯")               ++ "━┓\n"
          showLowerBorder       = " ┗" ++ concat    (replicate 7 "━┷")               ++ "━┛\n"
          showRowsSplitter      = " ┠" ++ concat    (replicate 7 "─┼")               ++ "─┨\n"
          showCells []     _    = ""
          showCells (c:cs) 1    = "┃" ++ show c ++ "┃"
                                      ++ show (8 - (snd . getCellCoordinates) c) -- invert Y coordinate
                                      ++ "\n"
                                      ++ ( if   (snd . getCellCoordinates) c < 7
                                           then showRowsSplitter
                                           else "" )
                                      ++ showCells cs 8
          showCells (c:cs) 8    = show (8 - (snd . getCellCoordinates) c) ++
                                  "┃" ++ show c ++ showCells cs 7
          showCells (c:cs) n    = "┃" ++ show c ++ showCells cs (n-1)


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
initialBoard = Board [placePiece (k `mod` 8) (k `div` 8) | k <- [0..63]]
  where placePiece x y = Cell (x, y) (color x y) (maybePiece x y)
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

-- Initial game.
initialGame = Game initialBoard (Move White)

-- Piece functions.
getMaybePieceColor :: Maybe Piece -> Maybe Color
getMaybePieceColor (Just (Piece color _)) = Just color
getMaybePieceColor Nothing = Nothing

getPieceColor :: Piece -> Color
getPieceColor (Piece color _) = color

getPieceType :: Piece -> PieceType
getPieceType (Piece _ pieceType) = pieceType

-- Cell functions.
emptyCell :: CellCoords -> Color -> Cell
emptyCell t color = Cell t color Nothing

getCellColor :: Cell -> Color
getCellColor (Cell _ color _) = color

setCellColor :: Color -> Cell -> Cell
setCellColor color (Cell t _ mbp) = Cell t color mbp

getCellPiece :: Cell -> Maybe Piece
getCellPiece (Cell _ _ mbp) = mbp

-- State functions
getMoveColor :: State -> Maybe Color
getMoveColor (Move color) = Just color
getMoveColor _            = Nothing

-- Switch color to opposite function
switchColor :: Color -> Color
switchColor Black = White
switchColor White = Black

-- Switch color wrapped with Maybe.
switchMaybeColor :: Maybe Color -> Maybe Color
switchMaybeColor (Just Black) = Just White
switchMaybeColor (Just White) = Just Black
switchMaybeColor Nothing      = Nothing

-- Game board functions.
getCellCoordinates :: Cell -> CellCoords
getCellCoordinates (Cell t _ _) = t

getMove :: Cell -> Cell -> Move
getMove (Cell (x,y) _ _) (Cell (x',y') _ _) = (x,y,x',y')

getCell :: Board -> CellCoords -> Cell
getCell (Board (x:xs)) t = if   t == getCellCoordinates x
                           then x
                           else getCell (Board xs) t
getCell (Board [])     t = error "Could not find anything on empty board!"

filterBoard :: (Cell -> Bool) -> Board -> Board
filterBoard _    (Board [])             = Board []
filterBoard pred (Board (cell:cells))   = if   pred cell
                                          then appendCell (filterBoard pred (Board cells)) cell
                                          else filterBoard pred (Board cells)
  where appendCell (Board cells_) cell_ = Board (cell_ : cells_)

boardSize :: Board -> Int
boardSize (Board cells) = length cells

-- Route on game board type
type Route = [Cell]

-- Find vertical, horizontal or diagonal route function.
-- Finishing cell will be excluded.
buildVerticalRoute :: Board -> Move -> Route
buildVerticalRoute board (x,y,x',y') | y'-y > 0  = cell : buildVerticalRoute board (x,y+1,x',y')
                                     | y'-y < 0  = cell : buildVerticalRoute board (x,y-1,x',y')
                                     | otherwise = [] where cell = getCell board (x,y)

buildHorizontalRoute :: Board -> Move -> Route
buildHorizontalRoute board (x,y,x',y') | x'-x > 0  = cell : buildHorizontalRoute board (x+1,y,x',y')
                                       | x'-x < 0  = cell : buildHorizontalRoute board (x-1,y,x',y')
                                       | otherwise = [] where cell = getCell board (x,y)

buildDiagonalRoute :: Board -> Move -> Route
buildDiagonalRoute board (x,y,x',y') | y'-y > 0 && x'-x > 0 = cell : buildDiagonalRoute board (x+1,y+1,x',y')
                                     | y'-y < 0 && x'-x > 0 = cell : buildDiagonalRoute board (x+1,y-1,x',y')
                                     | y'-y > 0 && x'-x < 0 = cell : buildDiagonalRoute board (x-1,y+1,x',y')
                                     | y'-y < 0 && x'-x < 0 = cell : buildDiagonalRoute board (x-1,y-1,x',y')
                                     | otherwise            = [] where cell = getCell board (x,y)

-- Build route excluding starting and finishing cells.
buildRoute :: Board -> Move -> Route
buildRoute board move@(x,y,x',y') | x == x' && y /= y'           = tail $ buildVerticalRoute   board move
                                  | y == y' && x /= x'           = tail $ buildHorizontalRoute board move
                                  | abs (x - x') == abs (y - y') = tail $ buildDiagonalRoute   board move
                                  | otherwise                    = [] -- Only straight diagonal routes are allowed!

-- Check if route is clean.
isCleanRoute :: Route -> Bool
isCleanRoute = foldr ((&&) . isCellNothing) True

setCell :: CellCoords -> Cell -> Board -> Board
setCell (x,y) cell (Board cells) = Board (replace (x,y) cell cells)
  -- Replace list element function.
  where replace :: CellCoords -> Cell -> [Cell] -> [Cell]
        replace t newElement (x:xs) = if   t == getCellCoordinates x
                                      then newElement : xs 
                                      else x : replace t newElement xs
        replace _ _           []    = error "Could not replace element on empty board!"

isCellNothing :: Cell -> Bool
isCellNothing (Cell _ _ mbp) = isNothing mbp

-- Game type functions.
getGameBoard :: Game -> Board
getGameBoard (Game board _) = board

getGameState :: Game -> State
getGameState (Game _ state) = state

isGameFinished :: Game -> Bool
isGameFinished (Game _ state) = state `elem` [ Draw
                                             , CheckMate White
                                             , CheckMate Black ]

-- Transform game state function.
isMoveAllowed :: Color -> Board -> Move -> Bool
isMoveAllowed moveColor board@(Board cells) move@(x,y,x',y') =
  isJust sourceMaybePiece && moveColor == pieceColor && case (moveColor, pieceType) of
         (White, Pawn) -> ( if   y  == 6
                            then dx == 0 && 0 > dy && dy > -3
                            else dx == 0 && 0 > dy && dy > -2
                       &&   isNothing destMaybePiece && isCleanRoute (buildRoute board move) )
                       || ( isJust destMaybePiece && (moveColor /= destPieceColor) && dy == -1 && absDx == 1 )
         (Black, Pawn) -> ( if   y  == 1
                            then dx == 0 && 0 < dy && dy < 3
                            else dx == 0 && 0 < dy && dy < 2
                       &&   isNothing destMaybePiece && isCleanRoute (buildRoute board move) )
                       || ( isJust destMaybePiece && (moveColor /= destPieceColor) && dy == 1 && absDx == 1 )
         (_, Rook  )   -> ( isNothing destMaybePiece || destPieceColor /= moveColor )
                         && isCleanRoute (buildRoute board move) && (abs (x - x') == 0 || abs (y - y') == 0)
         (_, Knight)   -> ( isNothing destMaybePiece || destPieceColor /= moveColor ) && l1Dist == 3
         (_, Bishop)   -> ( isNothing destMaybePiece || destPieceColor /= moveColor )
                         && isCleanRoute (buildRoute board move) && absDx == absDy
         (_, King  )   -> ( isNothing destMaybePiece || destPieceColor /= moveColor )
                         && isCleanRoute (buildRoute board move) && 0 < l1Dist && l1Dist < 3
         (_, Queen )   -> ( isNothing destMaybePiece || destPieceColor /= moveColor )
                         && isCleanRoute (buildRoute board move) && ( absDx == absDy
                                                                   || 0 < l1Dist && l1Dist < 3
                                                                   || absDx == 0
                                                                   || absDy == 0 )
    where sourceMaybePiece = getCellPiece  $ getCell board (x, y)
          pieceType        = getPieceType  $ fromMaybe (error "Could not get piece type of Nothing!")
                                             sourceMaybePiece
          pieceColor       = getPieceColor $ fromMaybe (error "Could not get piece color of Nothing!")
                                             sourceMaybePiece
          destMaybePiece   = getCellPiece  $ getCell board (x',y')
          destPieceColor   = fromMaybe (error "Could not get piece color because cell was empty!")
                                       (getMaybePieceColor destMaybePiece)
          l1Dist = abs (x - x') + abs (y - y')
          dx     = x' - x
          dy     = y' - y
          absDx  = abs dx
          absDy  = abs dy

transformGame :: Game -> Move -> Game
transformGame game@(Game board state) move@(x,y,x',y') = Game (transformBoard board state)
                                                              (transformState state)
  where transformState state@(Move color) = if   canMove move state
                                            then Move (switchColor color) -- TODO add checks, checkmates and draws
                                            else state
        transformState state              = state
        maybePieceColorOnSrcCell = getMaybePieceColor (getCellPiece srcCell)
        srcCell                  = getCell board (x,  y )
        destCell                 = getCell board (x', y')
        transformBoard board state@(Move color) = if   canMove move state
                                                  then setCell (x', y') newDestCell $
                                                       setCell (x,  y ) newSrcCell board
                                                  else board
                                                  where newSrcCell  = emptyCell (getCellCoordinates srcCell)
                                                                                (getCellColor srcCell)
                                                        newDestCell = Cell (getCellCoordinates destCell)
                                                                           (getCellColor destCell)
                                                                           (getCellPiece srcCell)
        transformBoard board _                  = board
        canMove move@(x,y,x',y') (Move color)   = abs (x - x') + abs (y - y') > 0 &&
                                                  isMoveAllowed color board move

makeMove :: Game -> Move -> Game
makeMove game@(Game board (CheckMate White)) _ = game
makeMove game@(Game board (CheckMate Black)) _ = game
makeMove game@(Game board Draw)              _ = game
makeMove game@(Game _     (Move _))       move = transformGame game move
