{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TicTacToe.Internal where

import Data.List (intersperse, intercalate)
import Data.Maybe (isJust)

data Symbol = Cross | Nought deriving (Eq, Show)

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

symbolToChar :: Symbol -> Char
symbolToChar Nought = 'o'
symbolToChar Cross = 'x'

printBoard :: Board -> IO ()
printBoard (Board rs) = do
  let indices = [0..length rs - 1]
  putStrLn (intercalate " " $ map show indices)
  mapM_ (putStrLn . printRow) $ zip indices rs
  where
    printRow (i, row) = (intersperse ' ' $ map (maybe '_' symbolToChar) row) ++ " " ++ show i

gameOver :: Board -> Either (Maybe Symbol) ()
gameOver b@(Board rs) = foldAllSame (rows b) >>
                        foldAllSame (cols b) >>
                        allSame (leftDiag b) >>
                        allSame (rightDiag b) >>
                        if all (all isJust) rs then Left Nothing else Right ()

foldAllSame :: Eq a => [[Maybe a]] -> Either (Maybe a) ()
foldAllSame = foldr (\x y -> allSame x >> y) (Right ())

allSame :: Eq a => [Maybe a] -> Either (Maybe a) ()
allSame [] = Left Nothing
allSame (x:xs) = if (isJust x) && all (== x) xs then Left x else Right ()

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
