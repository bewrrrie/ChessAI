module ChessLib where

import           Data.Maybe (fromMaybe, isJust, isNothing)

data Color
  = White
  | Black
  deriving (Eq, Show)

-- Chess piece type.
data PieceType
  = Pawn
  | Rook
  | Knight
  | Bishop
  | Queen
  | King
  deriving (Eq, Show)

data Piece =
  Piece Color PieceType
  deriving (Eq)

-- Show instance for pieces:
instance Show Piece where
  show (Piece White Pawn)   = "♙"
  show (Piece White Rook)   = "♖"
  show (Piece White Knight) = "♘"
  show (Piece White Bishop) = "♗"
  show (Piece White Queen)  = "♕"
  show (Piece White King)   = "♔"
  show (Piece Black Pawn)   = "♟"
  show (Piece Black Rook)   = "♜"
  show (Piece Black Knight) = "♞"
  show (Piece Black Bishop) = "♝"
  show (Piece Black Queen)  = "♛"
  show (Piece Black King)   = "♚"

-- Game board cell type.
type CellCoords = (Int, Int)

type Move = (Int, Int, Int, Int)

data Cell =
  Cell CellCoords Color (Maybe Piece)
  deriving (Eq)

instance Show Cell where
  show (Cell _ _ (Just piece)) = show piece
  show (Cell _ White Nothing)  = "▫"
  show (Cell _ Black Nothing)  = "▪"

-- Game board type.
newtype Board =
  Board [Cell]
  deriving (Eq)

-- String representation of game board.
instance Show Board where
  show (Board cells) =
    "\n" ++ showLetterCoordinates ++ showUpperBorder ++ showCells cells 8 ++ showLowerBorder ++ showLetterCoordinates
    where
      showLetterCoordinates = " " ++ concatMap (\x -> " " ++ [x]) ['A' .. 'H'] ++ "\n"
      showUpperBorder = " ┏" ++ concat (replicate 7 "━┯") ++ "━┓\n"
      showLowerBorder = " ┗" ++ concat (replicate 7 "━┷") ++ "━┛\n"
      showRowsSplitter = " ┠" ++ concat (replicate 7 "─┼") ++ "─┨\n"
      showCells [] _ = ""
      showCells (c:cs) 1 =
        "│" ++
        show c ++
        "┃" ++
        show (8 - (snd . getCellCoordinates) c) -- invert Y coordinate
         ++
        "\n" ++
        (if (snd . getCellCoordinates) c < 7
           then showRowsSplitter
           else "") ++
        showCells cs 8
      showCells (c:cs) 8 = show (8 - (snd . getCellCoordinates) c) ++ "┃" ++ show c ++ showCells cs 7
      showCells (c:cs) n = "│" ++ show c ++ showCells cs (n - 1)

-- Chess game state type.
data State
  = Move Color
  | Check Color
  | Mate Color
  | Draw
  | CheckMate Color
  deriving (Eq)

instance Show State where
  show (Move White)      = "Move for White."
  show (Move Black)      = "Move for Black."
  show Draw              = "Draw!"
  show (Check White)     = "White is in check."
  show (Check Black)     = "Black is in check."
  show (CheckMate White) = "White is in checkmate."
  show (CheckMate Black) = "Black is in checkmate."

data Game =
  Game Board State
  deriving (Eq)

instance Show Game where
  show (Game board state) = show board ++ "\n" ++ show state

-- Pieces valuation systems.
-- | Pieces standard valuation function.
stdValue :: PieceType -> Float
stdValue Pawn   = 1
stdValue Knight = 3
stdValue Bishop = 3
stdValue Rook   = 5
stdValue Queen  = 9
stdValue King   = 10 ^ 10

-- | Crafty pieces valuation function.
--   (  http://www.craftychess.com/  )
craftyValue :: PieceType -> Float
craftyValue Pawn   = 100
craftyValue Knight = 325
craftyValue Bishop = 325
craftyValue Rook   = 500
craftyValue Queen  = 1050
craftyValue King   = 40000

-- | Stockfish pieces valuation function.
stockfishEndGameValue :: PieceType -> Float
stockfishEndGameValue Pawn   = 1
stockfishEndGameValue Knight = 4.16
stockfishEndGameValue Bishop = 4.41
stockfishEndGameValue Rook   = 6.625
stockfishEndGameValue Queen  = 12.92
stockfishEndGameValue King   = 3

-- Black pieces.
blackPawn, blackRook, blackKnight, blackBishop, blackQueen, blackKing :: Piece
blackPawn = Piece Black Pawn

blackRook = Piece Black Rook

blackKnight = Piece Black Knight

blackBishop = Piece Black Bishop

blackQueen = Piece Black Queen

blackKing = Piece Black King

-- White pieces.
whitePawn, whiteRook, whiteKnight, whiteBishop, whiteQueen, whiteKing :: Piece
whitePawn = Piece White Pawn

whiteRook = Piece White Rook

whiteKnight = Piece White Knight

whiteBishop = Piece White Bishop

whiteQueen = Piece White Queen

whiteKing = Piece White King

-- Initial game board state.
initialBoard :: Board
initialBoard = Board [placePiece (k `mod` 8) (k `div` 8) | k <- [0 .. 63]]
  where
    placePiece x y = Cell (x, y) (color x y) (maybePiece x y)
    color x y
      | (x + y) `mod` 2 == 0 = White
      | otherwise = Black
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
getMaybePieceColor Nothing                = Nothing

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

-- | Switch color to opposite.
switchColor :: Color -> Color
switchColor Black = White
switchColor White = Black

-- | Switch color wrapped with Maybe to opposite.
switchMaybeColor :: Maybe Color -> Maybe Color
switchMaybeColor (Just Black) = Just White
switchMaybeColor (Just White) = Just Black
switchMaybeColor Nothing      = Nothing

-- Game board functions.
getCellCoordinates :: Cell -> CellCoords
getCellCoordinates (Cell t _ _) = t

getMove :: Cell -> Cell -> Move
getMove (Cell (x, y) _ _) (Cell (x', y') _ _) = (x, y, x', y')

getCell :: Board -> CellCoords -> Cell
getCell (Board (x:xs)) t =
  if t == getCellCoordinates x
    then x
    else getCell (Board xs) t
getCell (Board []) t = error "Could not find anything on empty board!"

filterBoard :: (Cell -> Bool) -> Board -> Board
filterBoard _ (Board []) = Board []
filterBoard pred (Board (cell:cells)) =
  if pred cell
    then appendCell (filterBoard pred (Board cells)) cell
    else filterBoard pred (Board cells)
  where
    appendCell (Board cells_) cell_ = Board (cell_ : cells_)

boardSize :: Board -> Int
boardSize (Board cells) = length cells

-- Route on game board type
type Route = [Cell]

-- | Find vertical route.
--   Finishing cell will be excluded.
buildVerticalRoute :: Board -> Move -> Route
buildVerticalRoute board (x, y, x', y')
  | y' - y > 0 = cell : buildVerticalRoute board (x, y + 1, x', y')
  | y' - y < 0 = cell : buildVerticalRoute board (x, y - 1, x', y')
  | otherwise = []
  where
    cell = getCell board (x, y)

-- | Find horizontal route.
--   Finishing cell will be excluded.
buildHorizontalRoute :: Board -> Move -> Route
buildHorizontalRoute board (x, y, x', y')
  | x' - x > 0 = cell : buildHorizontalRoute board (x + 1, y, x', y')
  | x' - x < 0 = cell : buildHorizontalRoute board (x - 1, y, x', y')
  | otherwise = []
  where
    cell = getCell board (x, y)

-- | Find diagonal route.
--   Finishing cell will be excluded.
buildDiagonalRoute :: Board -> Move -> Route
buildDiagonalRoute board (x, y, x', y')
  | y' - y > 0 && x' - x > 0 = cell : buildDiagonalRoute board (x + 1, y + 1, x', y')
  | y' - y < 0 && x' - x > 0 = cell : buildDiagonalRoute board (x + 1, y - 1, x', y')
  | y' - y > 0 && x' - x < 0 = cell : buildDiagonalRoute board (x - 1, y + 1, x', y')
  | y' - y < 0 && x' - x < 0 = cell : buildDiagonalRoute board (x - 1, y - 1, x', y')
  | otherwise = []
  where
    cell = getCell board (x, y)

-- | Build route excluding starting and finishing cells.
--   Based on given move.
--   Non-diagonal, non-vertical and non-horizontal routes will be ignored
--   and will lead to empty route as result.
buildRoute :: Board -> Move -> Route
buildRoute board move@(x, y, x', y')
  | x == x' && y /= y' = tail $ buildVerticalRoute board move
  | y == y' && x /= x' = tail $ buildHorizontalRoute board move
  | abs (x - x') == abs (y - y') = tail $ buildDiagonalRoute board move
  | otherwise = [] -- Only straight diagonal routes are allowed!

-- | Check if given route is clean.
isCleanRoute :: Route -> Bool
isCleanRoute = foldr ((&&) . isCellNothing) True

setCell :: CellCoords -> Cell -> Board -> Board
setCell (x, y) cell (Board cells) = Board (replace (x, y) cell cells)
  -- Replace list element function.
  where
    replace :: CellCoords -> Cell -> [Cell] -> [Cell]
    replace t newElement (x:xs) =
      if t == getCellCoordinates x
        then newElement : xs
        else x : replace t newElement xs
    replace _ _ [] = error "Could not replace element on empty board!"

isCellNothing :: Cell -> Bool
isCellNothing (Cell _ _ mbp) = isNothing mbp

-- Game type functions.
getGameBoard :: Game -> Board
getGameBoard (Game board _) = board

getGameState :: Game -> State
getGameState (Game _ state) = state

isGameFinished :: Game -> Bool
isGameFinished (Game _ state) = state `elem` [Draw, CheckMate White, CheckMate Black]

-- Transform game state functions.
-- | Check if specified move is allowed by game logic.
--   Color     is a pieces color of player that going to make a move.
--   Board     is a given game board state.
--   Move      is a tuple of four integers (Int,Int,Int,Int).
--   This function returns boolean that tells us
--   if specified move of player of specified color is allowed be done.
isMoveAllowed :: Color -> Board -> Move -> Bool
isMoveAllowed moveColor board@(Board cells) move@(x, y, x', y') =
  isJust sourceMaybePiece &&
  moveColor == pieceColor &&
  l1Dist > 0 &&
  case (moveColor, pieceType) of
    (White, Pawn) ->
      (if y == 6
         then dx == 0 && 0 > dy && dy > -3
         else dx == 0 && 0 > dy && dy > -2) &&
      isNothing destMaybePiece && isCleanRoute route ||
      (isJust destMaybePiece && moveColor /= destPieceColor && dy == -1 && absDx == 1)
    (Black, Pawn) ->
      (if y == 1
         then dx == 0 && 0 < dy && dy < 3
         else dx == 0 && 0 < dy && dy < 2) &&
      isNothing destMaybePiece && isCleanRoute route ||
      (isJust destMaybePiece && moveColor /= destPieceColor && dy == 1 && absDx == 1)
    (_, Rook) ->
      (isNothing destMaybePiece || destPieceColor /= moveColor) &&
      isCleanRoute route && (abs (x - x') == 0 || abs (y - y') == 0)
    (_, Knight) -> (isNothing destMaybePiece || destPieceColor /= moveColor) && l1Dist == 3 && absDx < 3 && absDy < 3
    (_, Bishop) -> (isNothing destMaybePiece || destPieceColor /= moveColor) && isCleanRoute route && absDx == absDy
    (_, King) ->
      (isNothing destMaybePiece || destPieceColor /= moveColor) && isCleanRoute route && 0 < l1Dist && l1Dist < 2
    (_, Queen) ->
      (isNothing destMaybePiece || destPieceColor /= moveColor) &&
      isCleanRoute route && (0 < l1Dist && l1Dist < 3 || absDx == absDy || absDx == 0 || absDy == 0)
  where
    route = buildRoute board move
    sourceMaybePiece = getCellPiece $ getCell board (x, y)
    pieceType = getPieceType $ fromMaybe (error "Could not get piece type of Nothing!") sourceMaybePiece
    pieceColor = getPieceColor $ fromMaybe (error "Could not get piece color of Nothing!") sourceMaybePiece
    destMaybePiece = getCellPiece $ getCell board (x', y')
    destPieceColor =
      fromMaybe (error "Could not get piece color because cell was empty!") (getMaybePieceColor destMaybePiece)
    l1Dist = abs (x - x') + abs (y - y')
    dx = x' - x
    dy = y' - y
    absDx = abs dx
    absDy = abs dy

-- | Transform game board according to piece move.
transformBoard :: State -> Move -> Board -> Board
transformBoard Draw _ board = board
transformBoard (Check _) _ board = board
transformBoard (CheckMate _) _ board = board
transformBoard state@(Move color) move@(x, y, x', y') board =
  if isMoveAllowed color board move
    then setCell (x', y') newDestCell $ setCell (x, y) newSrcCell board
    else board
  where
    newSrcCell = emptyCell (getCellCoordinates srcCell) (getCellColor srcCell)
    newDestCell = Cell (getCellCoordinates destCell) (getCellColor destCell) (getCellPiece srcCell)
    srcCell = getCell board (x, y)
    destCell = getCell board (x', y')

hasThreatToKing :: Color -> Board -> Bool
hasThreatToKing color board = False --TODO check if opponent threaten to our king

hasMove :: Color -> Board -> Bool
hasMove color board@(Board cells) = or canMoveMatrix
  where
    canMoveMatrix = [isMoveAllowed color board (x, y, x', y') | (x, y) <- coords, (x', y') <- coords]
    coords = [(i, j) | i <- [0 .. n - 1], j <- [1 .. n - 1]]
    n = length cells

-- | Transform game state according to piece move.
transformState :: Board -> Board -> State -> State
transformState oldBoard newBoard state =
  if oldBoard == newBoard
    then state
    else getNewState newBoard state
  where
    getNewState board state@(Move color)
      | canNotMove && kingThreat = CheckMate newColor
      | kingThreat = Check newColor
      | canNotMove = Draw
      | otherwise = Move newColor
      where
        newColor = switchColor color
        kingThreat = hasThreatToKing color board
        canNotMove = not $ hasMove color board

-- | Transform game object according to piece move.
--   Transform board and game state.
transformGame :: Game -> Move -> Game
transformGame game@(Game oldBoard state) move@(x, y, x', y') = Game newBoard newState
  where
    newBoard = transformBoard state move oldBoard
    newState = transformState oldBoard newBoard state

-- | Function that encapsulate all chess moves logic.
--   Used for whole game state transformation.
makeMove :: Game -> Move -> Game
makeMove game@(Game board (CheckMate White)) _ = game
makeMove game@(Game board (CheckMate Black)) _ = game
makeMove game@(Game board Draw) _              = game
makeMove game@(Game _ (Move _)) move           = transformGame game move
