module TicTacToe (
  Symbol (..),
  Board,
  newBoard,
  Player (..),
  Position (..),
  Move (..),
  playMove,
  gameOver,
  printBoard,
  parseSymbol,
) where

import TicTacToe.Internal

data Player = One | Two

data Move = Move Symbol Position deriving Show

playMove :: Board -> Move -> Either Error Board
playMove b (Move s p)
  | valid p b = Right (setCell s p b)
  | otherwise = Left InvalidPosition

parseSymbol :: String -> Either String Symbol
parseSymbol "x" = Right Cross
parseSymbol "o" = Right Nought
parseSymbol s = Left (s ++ " is not a symbol")
