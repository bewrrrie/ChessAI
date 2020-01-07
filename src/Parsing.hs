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
  | 'b' == toLower (head s) = Just Black
  | otherwise = Nothing

-- Convert string to tuple of four integers where
-- first two integers are coordinates of pieces that is going to be moved
-- and second pair of integers are coordinates of square where is the piece
-- going to be placed after move.
parseMove :: String -> (Maybe Int, Maybe Int, Maybe Int, Maybe Int)
parseMove (a:b:c:d:_) = ( elemIndex (toLower a) ['a' .. 'h']
                        , transformAndCheck b
                        , elemIndex (toLower c) ['a' .. 'h']
                        , transformAndCheck d )
  where errMessage char = "Could not parse coordinate '"
                          ++ [char] ++ "'!"
        transformAndCheck char = if   1 <= intCoordinate && intCoordinate <= 8
                                 then Just (8 - intCoordinate)
                                 else Nothing
          where intCoordinate = if    char `elem` ['1'..'8']
                                then (read::String->Int) [char]
                                else  -1
parseMove _           = (Nothing, Nothing, Nothing, Nothing)

-- Check if command to quit was written
parseQuit :: String -> Bool
parseQuit = (== 'q') . toLower . head
