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
       \(ValidMove move size) -> (let isSet s p b = getCell b p == Just (Just s) in
                                    playMove (newBoard size) move
                                   `shouldSatisfy`
                                   case move of
                                     (Move s p) -> fromRight undefined . fmap (isSet s p))

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
    size <- chooseInt (2, 4)
    row <- chooseInt (0, size - 1)
    col <- chooseInt (0, size - 1)
    return (ValidPosition (Position row col) size)