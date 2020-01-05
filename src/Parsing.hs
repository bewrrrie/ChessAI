module Parsing where

import ChessLib
import Data.List (isPrefixOf, elemIndex)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)

-- Parsing functions.
-- Convert string to color type.
parseColor :: String -> Maybe Color
parseColor s
  | isPrefixOf "w" $ map toLower s = Just White
  | isPrefixOf "b" $ map toLower s = Just White
  | otherwise = Nothing

-- Convert string to tuple of four integers where
-- first two integers are coordinates of pieces that is going to be moved
-- and second pair of integers are coordinates of square where is the piece
-- going to be placed after move.
parseMove :: String -> (Int, Int, Int, Int)
parseMove (a:b:c:d:_) = ( fromMaybe (error $ errMessage a) $
                          elemIndex (toLower a) ['a' .. 'h']
                        , transformAndCheck b - 1
                        , fromMaybe (error $ errMessage c) $
                          elemIndex (toLower c) ['a' .. 'h']
                        , transformAndCheck d - 1 )
  where errMessage char = "Could not parse coordinate '"
                          ++ [char] ++ "'!"
        transformAndCheck char = if -1 < intCoordinate && intCoordinate < 8
                                 then intCoordinate
                                 else error (errMessage char)
          where intCoordinate = (read::String->Int) [char]
