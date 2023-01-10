module TicTacToe (
  Symbol (..),
  Board,
  newBoard,
  Position (..),
  Move (..),
  playMove,
  gameOver,
  symbolToChar,
  printBoard,
  parseSymbol,
  otherSymbol,
  GameState (..),
) where

import TicTacToe.Internal

data GameState = GameState {
  currentSymbol :: Symbol,
  currentBoard :: Board,
  numMoves :: Int
  }

data Move = Move Symbol Position deriving Show

playMove :: Board -> Move -> Either Error Board
playMove b (Move s p)
  | valid p b = Right (setCell s p b)
  | otherwise = Left InvalidPosition

parseSymbol :: String -> Either String Symbol
parseSymbol "x" = Right Cross
parseSymbol "o" = Right Nought
parseSymbol s = Left (s ++ " is not a symbol")

otherSymbol :: Symbol -> Symbol
otherSymbol Nought = Cross
otherSymbol Cross = Nought
