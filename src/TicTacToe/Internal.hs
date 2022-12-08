{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TicTacToe.Internal where

import Data.List (intersperse)

data Symbol = Cross | Knot deriving (Eq, Show)

newtype Board = Board [[Maybe Symbol]] deriving (Show)

newBoard :: Int -> Board
newBoard size = Board (replicate size (replicate size Nothing))

data Position = Position Int Int deriving (Show)

valid :: Position -> Board -> Bool
valid (Position r c) (Board b) = (r >=0 &&
                                  r < length b &&
                                  c >=0 &&
                                  c < length (b !! r) &&
                                  ((b !! r) !! c) == Nothing)

-- Assumes that the position is valid
setCell :: Symbol -> Position -> Board -> Board
setCell s (Position r c) (Board b) = Board (setElem newRow r b)
  where
    newRow = setElem (Just s) c (b !! r)

setElem :: a -> Int -> [a] -> [a]
setElem a i as = take i as ++ (a : (drop i as))

getElem :: Int -> [a] -> Maybe a
getElem i as = if i < length as then Just (as !! i)
               else Nothing

getCell :: Board -> Position -> Maybe (Maybe Symbol)
getCell (Board b) (Position r c) = getElem r b >>= getElem c

printBoard :: Board -> IO ()
printBoard (Board rows) = mapM_ printRow rows
  where
    printRow cols = do
       putStrLn . intersperse ' ' $ map toChar cols
    toChar s = case s of
                 Nothing -> '_'
                 Just Knot -> 'o'
                 Just Cross -> 'x'
