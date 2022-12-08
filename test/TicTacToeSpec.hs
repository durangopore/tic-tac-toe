module TicTacToeSpec (
  spec
) where

import Data.Either

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import TicTacToe
import TicTacToe.Internal

spec :: Spec
spec = do
  testExecuteMove

testExecuteMove :: Spec
testExecuteMove = do
  describe "playMove" $ do
    prop "sets an arbitrary cell on an empty board if in range" $
      \(ValidMove move size) -> let isSet s p b = getCell b p == Just (Just s) in
                                  playMove' (newBoard' size) move
                                  `shouldSatisfy`
                                  case move of
                                    (Move s p) -> isSet s p
    prop "preserves size after a move is played" $
      \(ValidMove move size) -> playMove' (newBoard' size) move
                                `shouldSatisfy`
                                \board -> case board of
                                            (Board rows) -> length rows == size && all ((== size) . length) rows
  where
    newBoard' = fromRight undefined . newBoard
    playMove' b p = fromRight undefined $ playMove b p

data ValidMove = ValidMove Move Int deriving (Show)

instance Arbitrary ValidMove where
  arbitrary = do
    ValidPosition position size <- arbitrary
    AnySymbol symbol <- arbitrary
    return (ValidMove (Move symbol position) size)

newtype AnySymbol = AnySymbol Symbol deriving (Show)

instance Arbitrary AnySymbol where
  arbitrary = do
    elements $ map AnySymbol [Cross, Knot]

data ValidPosition = ValidPosition Position Int deriving (Show)

instance Arbitrary ValidPosition where
  arbitrary = do
    size <- arbitrary `suchThat` (> 2)
    row <- chooseInt (0, size - 1)
    col <- chooseInt (0, size - 1)
    return (ValidPosition (Position row col) size)
