module Model (
  Symbol (..),
  Board (..),
  Player (..),
  Position (..),
  Move (..)
  ) where

data Symbol = Cross | Knot deriving (Eq)

newtype Board = Board [[Maybe Symbol]]

data Player = One | Two

data Position = Position Int Int

data Move = Move Symbol Position
