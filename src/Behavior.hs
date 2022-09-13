module Behavior where

import Model

-- TODO: enrich the error type
executeMove :: Move -> Board -> Either String Board
executeMove (Move s p) b
  | valid p b = Right (setCell s p b)
  | otherwise = Left "invalid move"

valid :: Position -> Board -> Bool
valid (Position r c) (Board b) = (r >=0 &&
                                  r < length b &&
                                  c >=0 &&
                                  c < length (b !! r) &&
                                  ((b !! r) !! c) == Nothing)

setCell :: Symbol -> Position -> Board -> Board
setCell = undefined
