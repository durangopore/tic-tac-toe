module BehaviorSpec where

import Data.Either

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Behavior
import Model

spec :: Spec
spec = do
  testExecuteMove

testExecuteMove :: Spec
testExecuteMove = do
  describe "executeMove" $ do
    prop "sets an arbitrary cell on an empty board if in range" $
       \move size -> (executeMove move (newBoard size)
                       `shouldSatisfy`
                       case move of
                         (Move _ (Position row col)) | row < size && col < size ->  isRight
                         _ -> isLeft)

instance Arbitrary Move where
  arbitrary = do
    s <- arbitrary
    p <- arbitrary
    return (Move s p)

instance Arbitrary Symbol where
  arbitrary = do
    elements [Cross, Knot]

instance Arbitrary Position where
  arbitrary = do
    Positive row <- arbitrary
    Positive col <- arbitrary
    return $ Position row col
