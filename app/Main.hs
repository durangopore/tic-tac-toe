module Main (main) where

import Control.Exception (try)
import Data.Either.Combinators (mapLeft)
import System.Environment (getArgs)
import System.IO.Error (isEOFError)
import Text.Read (readEither)

import Lib

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Right size -> do
      case newBoard size of
        Right board -> do
          result <- selectionLoop
          case result of
            Left e -> putStrLn (if isEOFError e then "quit!" else ("there was an error: " ++ show e))
            Right symbol -> do
              gameResult <- gameLoop (initGameState board symbol)
              case gameResult of
                Left e -> putStrLn (if isEOFError e then "quit!" else ("there was an error: " ++ show e))
                _ -> return ()
        Left e -> putStrLn (show e)
    Left e -> putStrLn e

selectionLoop :: IO (Either IOError Symbol)
selectionLoop = do
  putStrLn "Select Player 1 symbol (x or o):"
  result <- try getLine
  case result of
    Left e -> return (Left e)
    Right input -> case parseSymbol input of
      Right symbol -> return (Right symbol)
      Left e       -> do
        putStrLn e
        selectionLoop

safeGetLine :: (String -> IO (Either IOError a)) -> IO (Either IOError a)
safeGetLine f = do
  result <- try getLine
  case result of
    Left e -> return (Left e)
    Right input -> f input

gameLoop :: GameState -> IO (Either IOError ())
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
      return (Right ())
    Left Nothing -> do
      putStrLn "Game Over, it's a draw!"
      return (Right ())
    Right () -> do
      safeGetLine (\input -> case parsePosition input of
        Right position -> do
          case playMove board (Move symbol position) of
            Right board' -> gameLoop (updateGameState board' gameState)
            Left playError -> do
              putStrLn (show playError)
              gameLoop gameState
        Left parseError -> do
          putStrLn parseError
          gameLoop gameState)

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
