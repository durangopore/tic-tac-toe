module Main (main) where

import Data.Either.Combinators (mapLeft)
import System.Environment (getArgs)
import Text.Read (readEither)

import Lib

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Right size -> do
      case newBoard size of
        Right board -> do
          symbol <- selectionLoop
          gameLoop GameState { currentBoard = board, currentSymbol = symbol }
        Left e -> putStrLn (show e)
    Left e -> putStrLn e

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
  putStrLn ("current player: " ++ [symbolToChar (Just symbol)])
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
  check [r, c] = mapLeft (const "Not an integer") $ do
    row <- readEither r
    col <- readEither c
    return (Position row col)
  check _ = Left "Too many or too few inputs"

parseArgs :: [String] -> Either String Int
parseArgs [arg] = mapLeft (const "Not an integer") (readEither arg)
parseArgs _ = Left "Invalid commandline arguments"
