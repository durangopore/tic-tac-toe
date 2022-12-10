{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TicTacToe.Internal where

import Data.List (intersperse)
import Data.Maybe (isJust)

data Symbol = Cross | Knot deriving (Eq, Show)

newtype Board = Board [[Maybe Symbol]] deriving Show

data Error = InvalidPosition | InvalidSize deriving Show

newBoard :: Int -> Either Error Board
newBoard size
  | size >= 2 = Right . Board $ replicate size (replicate size Nothing)
  | otherwise = Left InvalidSize

data Position = Position Int Int deriving Show

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
setElem a i as = take i as ++ (a : (drop (i + 1) as))

getElem :: Int -> [a] -> Maybe a
getElem i as = if i < length as then Just (as !! i)
               else Nothing

getCell :: Board -> Position -> Maybe (Maybe Symbol)
getCell (Board b) (Position r c) = getElem r b >>= getElem c

printBoard :: Board -> IO ()
printBoard (Board rs) = mapM_ (putStrLn . p) rs
  where
    p = intersperse ' ' . map toChar
    toChar s = case s of
                 Nothing -> '_'
                 Just Knot -> 'o'
                 Just Cross -> 'x'

gameOver :: Board -> Bool
gameOver b@(Board rs) = any allSame (rows b) ||
             any allSame (cols b) ||
             allSame (leftDiag b) ||
             allSame (rightDiag b) ||
             all (all isJust) rs

allSame :: Eq a => [Maybe a] -> Bool
allSame [] = True
allSame (x:xs) = isJust x && all (== x) xs

rows :: Board -> [[Maybe Symbol]]
rows (Board rs) = rs

cols :: Board -> [[Maybe Symbol]]
cols (Board rs) = f rs where
  f ([]:_) = []
  f xs = fmap head xs : f (fmap tail xs)

leftDiag :: Board -> [Maybe Symbol]
leftDiag (Board rs) = fmap f (zip rs [0..]) where
  f (xs, i)  = xs !! i

rightDiag :: Board -> [Maybe Symbol]
rightDiag (Board rs) = leftDiag (Board $ fmap reverse rs)
