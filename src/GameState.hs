{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE KindSignatures               #-}

module GameState (
  GameState,
  initGameState,
  updateGameState,
  currentSymbol,
  currentBoard,
  numMoves
) where

import Data.Nat (Nat (..))

import TicTacToe

data GameState (n :: Nat) = GameState (Board n) Symbol Int

initGameState :: Board n -> Symbol -> GameState n
initGameState board symbol = GameState board symbol 0

updateGameState :: Board n -> GameState n -> GameState n
updateGameState board (GameState _ symbol moves) = GameState board (otherSymbol symbol) (moves + 1)

currentSymbol :: GameState n -> Symbol
currentSymbol (GameState _ symbol _) = symbol

currentBoard :: GameState n -> Board n
currentBoard (GameState board _ _) = board

numMoves :: GameState n -> Int
numMoves (GameState _ _ moves) = moves
