module GameState (
  GameState,
  initGameState,
  updateGameState,
  currentSymbol,
  currentBoard,
  numMoves
) where

import TicTacToe

data GameState = GameState Board Symbol Int

initGameState :: Board -> Symbol -> GameState
initGameState board symbol = GameState board symbol 0

updateGameState :: Board -> GameState -> GameState
updateGameState board (GameState _ symbol moves) = GameState board (otherSymbol symbol) (moves + 1)

currentSymbol :: GameState -> Symbol
currentSymbol (GameState _ symbol _) = symbol

currentBoard :: GameState -> Board
currentBoard (GameState board _ _) = board

numMoves :: GameState -> Int
numMoves (GameState _ _ moves) = moves
