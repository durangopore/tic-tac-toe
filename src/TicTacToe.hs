{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE KindSignatures               #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE ExplicitForAll               #-}
{-# LANGUAGE RankNTypes                   #-}

module TicTacToe (
  Symbol (..),
  Board,
  newBoard,
  Position (..),
  Move (..),
  playMove,
  gameOver,
  symbolToChar,
  printBoard,
  parseSymbol,
  otherSymbol,
  Size (..),
  parseSize,
  Proxy (..),
  reify,
) where

import Data.Type.Nat (SNatI, Nat(..))

import TicTacToe.Internal

data Move = Move Symbol Position deriving Show

playMove :: SNatI n => Board n -> Move -> Either Error (Board n)
playMove b (Move s p)
  | valid p b = Right (setCell s p b)
  | otherwise = Left InvalidPosition

parseSymbol :: String -> Either String Symbol
parseSymbol "x" = Right Cross
parseSymbol "o" = Right Nought
parseSymbol s = Left (s ++ " is not a symbol")

otherSymbol :: Symbol -> Symbol
otherSymbol Nought = Cross
otherSymbol Cross = Nought

newtype Size = Size Int deriving Show

parseSize :: Int -> Either String Size
parseSize n
  | n < 2 = Left "Size must be >=2"
  | otherwise = Right (Size n)

data Proxy (n :: Nat) = Proxy

reify :: Size -> (forall n. SNatI n => Proxy n -> b) -> b
reify (Size size) = inner (size - 2)
  where
    -- assumes x > 0
    inner :: Int -> (forall n. SNatI n => Proxy n -> b) -> b
    inner 0 f = f (Proxy :: Proxy 'Z)
    inner x f = inner (x - 1) $ \(Proxy :: Proxy x_1) -> f (Proxy :: Proxy ('S x_1))

