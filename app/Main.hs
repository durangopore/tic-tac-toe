module Main (main) where

import Data.Either (fromRight)
import Text.Read (readEither)

import Lib

main :: IO ()
main = do
  symbol <- selectionLoop
  let board = fromRight undefined $ newBoard 3
  gameLoop GameState { currentBoard = board, currentSymbol = symbol }

selectionLoop :: IO Symbol
selectionLoop = do
  putStrLn "Select Player 1 symbol (x or o):"
  input <- getLine
  case parseSymbol input of
    Right symbol -> return symbol
    Left e       -> do
      putStrLn e
      selectionLoop

gameLoop :: GameState -> IO ()
gameLoop gameState = do
  let board = currentBoard gameState
  let symbol = currentSymbol gameState
  printBoard board
  if gameOver board then
    (putStrLn "Game Over")
  else do
    input <- getLine
    case parsePosition input of
      Right position -> do
        case playMove board (Move symbol position) of
          Right board' -> gameLoop GameState { currentBoard = board', currentSymbol = otherSymbol symbol }
          Left playError -> do
            putStrLn (show playError)
            gameLoop gameState
      Left parseError -> do
        putStrLn parseError
        gameLoop gameState

parsePosition :: String -> Either String Position
parsePosition input = check (words input) where
  check [r, c] = do
    row <- readEither r
    col <- readEither c
    return (Position row col)
  check _ = Left "Bad input"
