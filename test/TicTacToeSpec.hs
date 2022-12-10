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
  testGameOver

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
                                            (Board rs) -> length rs == size && all ((== size) . length) rs
    prop "preserves size after a move is played" $
      \(GameOver board) -> board `shouldSatisfy` gameOver
  where
    newBoard' = fromRight undefined . newBoard
    playMove' b p = fromRight undefined $ playMove b p

testGameOver :: Spec
testGameOver = do
  describe "gameOver" $ do
    prop "detects when the game is over" $
      \(GameOver board) -> board `shouldSatisfy` gameOver

data ValidMove = ValidMove Move Int deriving Show

instance Arbitrary ValidMove where
  arbitrary = do
    ValidPosition position size <- arbitrary
    AnySymbol symbol <- arbitrary
    return (ValidMove (Move symbol position) size)

newtype AnySymbol = AnySymbol Symbol deriving Show

instance Arbitrary AnySymbol where
  arbitrary = do
    elements $ map AnySymbol [Cross, Knot]

data ValidPosition = ValidPosition Position Int deriving Show

instance Arbitrary ValidPosition where
  arbitrary = do
    size <- arbitrary `suchThat` (> 2)
    row <- chooseInt (0, size - 1)
    col <- chooseInt (0, size - 1)
    return (ValidPosition (Position row col) size)

newtype GameOver = GameOver Board deriving Show

instance Arbitrary GameOver where
  arbitrary = do
    ValidPosition (Position rowIndex colIndex) size <- arbitrary
    Board board <- randomBoard size
    AnySymbol symbol <- arbitrary

    let rowBoard = setElem (replicate size (Just symbol)) rowIndex board
    let colBoard = setCol symbol colIndex board
    let leftDiagBoard = setLeftDiag symbol board
    let rightDiagBoard = setRightDiag symbol size board
    Board staleMate <- fullBoard size
    selectedBoard <- elements [staleMate, rowBoard, colBoard, leftDiagBoard, rightDiagBoard]
    return (GameOver (Board selectedBoard))

    where
      setCol symbol colIndex  = fmap (setElem (Just symbol) colIndex)
      setLeftDiag symbol = fmap f . zip [0..]
        where
          f (i, r) = setElem (Just symbol) i r
      setRightDiag symbol size = fmap f . zip [size-1..1]
        where
          f (i, r) = setElem (Just symbol) i r
      randomBoard size = do
        board <- vectorOf size . vectorOf size . elements $ [Nothing, Just Knot, Just Cross]
        return (Board (board))
      fullBoard size = do
        board <- vectorOf size . vectorOf size . elements $ [Just Knot, Just Cross]
        return (Board (board))
