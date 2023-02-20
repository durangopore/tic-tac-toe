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
  testNewBoard
  testPlayMove
  testGameOver
  testParseSymbol
  testOtherSymbol

testNewBoard :: Spec
testNewBoard = do
  describe "newBoard" $ do
    prop "handles any size correctly" $
      \size -> if size < 2 then
                 newBoard size `shouldSatisfy` isLeft
               else
                 newBoard size `shouldSatisfy` (hasSize size . fromRight undefined)

testPlayMove :: Spec
testPlayMove = do
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
                                hasSize size
  where
    newBoard' = fromRight undefined . newBoard
    playMove' b p = fromRight undefined $ playMove b p

hasSize :: Int -> Board -> Bool
hasSize size (Board rs) = length rs == size && all ((== size) . length) rs

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

data ValidMove = ValidMove Move Int deriving Show

instance Arbitrary ValidMove where
  arbitrary = do
    ValidPosition position size <- arbitrary
    AnySymbol symbol <- arbitrary
    return (ValidMove (Move symbol position) size)

newtype AnySymbol = AnySymbol Symbol deriving Show

instance Arbitrary AnySymbol where
  arbitrary = do
    elements $ map AnySymbol [Cross, Nought]

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
        board <- vectorOf size . vectorOf size . elements $ [Nothing, Just Nought, Just Cross]
        return (Board (board))
      fullBoard size = do
        board <- vectorOf size . vectorOf size . elements $ [Just Nought, Just Cross]
        return (Board (board))
