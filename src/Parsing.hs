module Parsing where

import ChessLib
import Data.List (isPrefixOf, elemIndex)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)

-- Parsing functions.
-- Convert string to color type.
parseColor :: String -> Maybe Color
parseColor s
  | 'w' == toLower (head s) = Just White
  | 'b' == toLower (head s) = Just White
  | otherwise = Nothing

-- Convert string to tuple of four integers where
-- first two integers are coordinates of pieces that is going to be moved
-- and second pair of integers are coordinates of square where is the piece
-- going to be placed after move.
parseMove :: String -> (Int, Int, Int, Int)
parseMove []          = error "Could not parse move from empty string!"
parseMove (a:b:c:d:_) = ( fromMaybe (error $ errMessage a) $
                          elemIndex (toLower a) ['a' .. 'h']
                        , 9 - transformAndCheck b
                        , fromMaybe (error $ errMessage c) $
                          elemIndex (toLower c) ['a' .. 'h']
                        , 9 - transformAndCheck d )
  where errMessage char = "Could not parse coordinate '"
                          ++ [char] ++ "'!"
        transformAndCheck char = if 1 <= intCoordinate && intCoordinate <= 8
                                 then intCoordinate
                                 else error (errMessage char)
          where intCoordinate = (read::String->Int) [char]

-- Check if command to quit was written
parseQuit :: String -> Bool
parseQuit = (== 'q') . toLower . head 
