module Main (main) where

import Data.Either (fromRight)
import Text.Read (readEither)

import Lib

main :: IO ()
main = gameLoop (fromRight undefined $ newBoard 3)

gameLoop :: Board -> IO ()
gameLoop board = do
  printBoard board
  if gameOver board then
    (putStrLn "Game Over")
  else do
    input <- getLine
    case parseInput input of
      Right move -> do
        case playMove board move of
          Right board' -> gameLoop board'
          Left playError -> do
            putStrLn (show playError)
            gameLoop board
      Left parseError -> do
        putStrLn parseError
        gameLoop board

parseInput :: String -> Either String Move
parseInput input = check (words input) where
  check [s, r, c] = do
    symbol <- parseSymbol s
    row <- readEither r
    col <- readEither c
    return (Move symbol (Position row col))
  check _ = Left "Bad input"
