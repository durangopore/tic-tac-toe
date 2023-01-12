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
          gameLoop (initGameState board symbol)
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
  let moves = numMoves gameState
  putStrLn ("number of moves: " ++ show moves)
  putStrLn ("current player: " ++ [symbolToChar symbol])
  printBoard board
  case gameOver board of
    Left (Just s) -> do
      putStrLn ("Game Over, " ++ [symbolToChar s] ++ " wins!")
    Left Nothing -> do
      putStrLn "Game Over, it's a draw!"
    Right () -> do
      input <- getLine
      case parsePosition input of
        Right position -> do
          case playMove board (Move symbol position) of
            Right board' -> gameLoop (updateGameState board' gameState)
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
