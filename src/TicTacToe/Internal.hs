{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE KindSignatures               #-}

module TicTacToe.Internal where

import Prelude hiding (length, take, drop, reverse)

import Data.List (intersperse, intercalate)
import Data.Maybe (isJust)
import Data.Nat (Nat (..))
import Data.Type.Nat (SNatI)
import Data.Vec.Lazy (Vec, tabulate, length, (!), toList, reverse)
import qualified Data.Vec.Lazy as V (map)

data Symbol = Cross | Nought deriving (Eq, Show)

data Board (n :: Nat) = Board (Vec ('S ('S n)) (Vec ('S ('S n)) (Maybe Symbol))) deriving Show

data Error = InvalidPosition deriving Show

newBoard :: SNatI n => proxy n -> Board n
newBoard _ = Board $ tabulate (const (tabulate (const Nothing)))

data Position = Position Int Int deriving Show

valid :: SNatI n => Position -> Board n -> Bool
valid (Position r c) (Board b) = (r >=0 &&
                                  r < length b &&
                                  c >=0 &&
                                  c < length (b ! toEnum r) &&
                                  ((b ! toEnum r) ! toEnum c) == Nothing)

-- Assumes that the position is valid
setCell :: SNatI n => Symbol -> Position -> Board n -> Board n
setCell s (Position r c) (Board b) = Board (setElem newRow r b)
  where
    newRow = setElem (Just s) c (b ! toEnum r)

setElem :: SNatI n => a -> Int -> Vec n a  -> Vec n a
setElem a i as = tabulate set
  where
    set index
      | index == toEnum i = a
      | otherwise = as ! index

getElem :: SNatI n => Int -> Vec n a -> Maybe a
getElem i as = if i < length as then Just (as ! toEnum i)
               else Nothing

getCell :: SNatI n => Board n -> Position -> Maybe (Maybe Symbol)
getCell (Board b) (Position r c) = getElem (toEnum r) b >>= getElem (toEnum c)

symbolToChar :: Symbol -> Char
symbolToChar Nought = 'o'
symbolToChar Cross = 'x'

printBoard :: Board n -> IO ()
printBoard (Board rs) = do
  let indices = [0..length rs - 1]
  putStrLn (intercalate " " $ map show indices)
  mapM_ (putStrLn . printRow) $ zip indices (toRowsList rs)
  where
    printRow (i, row) = (intersperse ' ' $ map (maybe '_' symbolToChar) row) ++ " " ++ show i

-- returns `Left` for a result and `Right` if the game is still on
gameOver :: Board n -> Either (Maybe Symbol) ()
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

rows :: Board n -> [[Maybe Symbol]]
rows (Board rs) = toRowsList rs

cols :: Board n -> [[Maybe Symbol]]
cols (Board rs) = f (toRowsList rs) where
  f ([]:_) = []
  f xs = fmap head xs : f (fmap tail xs)

leftDiag :: Board n -> [Maybe Symbol]
leftDiag (Board rs) = fmap f (zip (toRowsList rs) [0..]) where
  f (xs, i)  = xs !! i

rightDiag :: Board n -> [Maybe Symbol]
rightDiag (Board rs) = leftDiag (Board $ fmap reverse rs)

toRowsList :: Vec ('S ('S n)) (Vec ('S ('S n)) (Maybe Symbol)) -> [[Maybe Symbol]]
toRowsList = toList . V.map toList
