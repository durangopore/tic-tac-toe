module BehaviorSpec (
  spec,
  ) where

import Data.Either

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Behavior
import Model
import Model.Internal

spec :: Spec
spec = do
  testExecuteMove

testExecuteMove :: Spec
testExecuteMove = do
  describe "executeMove" $ do
    prop "sets an arbitrary cell on an empty board if in range" $
       \(AnyMove move) size -> (executeMove move (newBoard size)
                       `shouldSatisfy`
                       case move of
                         (Move s p@(Position row col)) | row < size && col < size -> f s p
                         _ -> isLeft)
      where
        f s p (Right b) = getCell b p == Just (Just s)
        f _ _ _ = False

newtype AnyMove = AnyMove Move deriving (Show)

instance Arbitrary AnyMove where
  arbitrary = do
    AnySymbol s <- arbitrary
    AnyPosition p <- arbitrary
    return (AnyMove $ Move s p)

newtype AnySymbol = AnySymbol Symbol deriving (Show)

instance Arbitrary AnySymbol where
  arbitrary = do
    elements $ map AnySymbol [Cross, Knot]

newtype AnyPosition = AnyPosition Position deriving (Show)

instance Arbitrary AnyPosition where
  arbitrary = do
    Positive row <- arbitrary
    Positive col <- arbitrary
    return (AnyPosition $ Position row col)
