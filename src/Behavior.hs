module Behavior (
  executeMove,
) where

import Model

data Error = InvalidMove deriving (Show)

executeMove :: Move -> Board -> Either Error Board
executeMove (Move s p) b
  | valid p b = Right (setCell s p b)
  | otherwise = Left InvalidMove
