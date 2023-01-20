{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE KindSignatures               #-}

module TicTacToeSpec (
  spec
) where

import Data.Either
import Data.Maybe (fromJust)
import Data.Nat (Nat (..))
import Data.Vec.Lazy (fromList)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import TicTacToe
import TicTacToe.Internal

spec :: Spec
spec = do
  testPlayMove
  testGameOver
  testParseSymbol
  testOtherSymbol

testPlayMove :: Spec
testPlayMove = do
  describe "playMove" $ do
    prop "sets an arbitrary cell on an empty board if in range" $
      \(ValidMove move size) -> reify size $ \p -> let isSet s p b = getCell b p == Just (Just s) in
                                                     playMove' (newBoard p) move
                                                     `shouldSatisfy`
                                                     case move of
                                                       (Move s p) -> isSet s p
  where
    playMove' b p = fromRight undefined $ playMove b p

testGameOver :: Spec
testGameOver = do
  describe "gameOver" $ do
    prop "detects when the game is over" $
      \(GameOver board) -> board `shouldSatisfy` (isLeft . gameOver)
    -- TODO: Validate the actual result (who won? draw?)

testParseSymbol :: Spec
testParseSymbol = do
  describe "parseSymbol" $ do
    prop "round trip works" $
      \(AnySymbol symbol) -> parseSymbol [symbolToChar symbol] == Right symbol
    it "fails to parse garbage" $
      parseSymbol "some random text" `shouldSatisfy` isLeft

testOtherSymbol :: Spec
testOtherSymbol = do
  describe "otherSymbol" $ do
    prop "round trip works" $
      \(AnySymbol symbol) -> symbol == otherSymbol (otherSymbol symbol)
    prop "toggle works" $
      \(AnySymbol symbol) -> symbol /= otherSymbol symbol

data ValidMove = ValidMove Move Size deriving Show

instance Arbitrary ValidMove where
  arbitrary = do
    ValidPosition position size <- arbitrary
    AnySymbol symbol <- arbitrary
    return (ValidMove (Move symbol position) size)

newtype AnySymbol = AnySymbol Symbol deriving Show

instance Arbitrary AnySymbol where
  arbitrary = do
    elements $ map AnySymbol [Cross, Nought]

data ValidPosition = ValidPosition Position Size deriving Show

instance Arbitrary ValidPosition where
  arbitrary = do
    size <- arbitrary `suchThat` (> 2)
    row <- chooseInt (0, size - 1)
    col <- chooseInt (0, size - 1)
    return (ValidPosition (Position row col) (Size size))

data GameOver (n :: Nat) = GameOver (Board n) deriving Show

instance Arbitrary (GameOver n) where
  arbitrary = do
    ValidPosition (Position rowIndex colIndex) (Size size) <- arbitrary
    reify (Size size) $ \p -> do
      board <- randomBoard size
      AnySymbol symbol <- arbitrary
      let rowBoard = setElemList (replicate size (Just symbol)) rowIndex board
          colBoard = setCol symbol colIndex board
          leftDiagBoard = setLeftDiag symbol board
          rightDiagBoard = setRightDiag symbol size board
      staleMate <- fullBoard size
      selectedBoard <- elements [staleMate, rowBoard, colBoard, leftDiagBoard, rightDiagBoard]
      return (GameOver (toBoard p selectedBoard))

    where
      setCol symbol colIndex  = fmap (setElemList (Just symbol) colIndex)
      setLeftDiag symbol = fmap f . zip [0..]
        where
          f (i, r) = setElemList (Just symbol) i r
      setRightDiag symbol size = fmap f . zip [size-1..1]
        where
          f (i, r) = setElemList (Just symbol) i r
      randomBoard size = do
        vectorOf size . vectorOf size . elements $ [Nothing, Just Nought, Just Cross]
      fullBoard size = do
        vectorOf size . vectorOf size . elements $ [Just Nought, Just Cross]
      toBoard :: proxy n -> [[Maybe Symbol]] -> Board n
      toBoard _ = Board . fromJust . fromList . fmap (fromJust . fromList)
      setElemList a i as = take i as ++ (a : (drop (i + 1) as))

