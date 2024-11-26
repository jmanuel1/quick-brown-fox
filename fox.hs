{-# LANGUAGE BinaryLiterals #-}

import Data.Bits
import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Data.Word
import Numeric

choose n k = product [n - k + 1 .. n] `quot` product [1 .. k]

fs = 5

os = 6

xs = 5

foxCombos = choose 16 fs * choose (16 - fs) os -- * choose (16 - fs - os) xs

ofxCombos = choose 16 os * choose (16 - os) fs -- * choose (16 - os - fs) xs

type Board = Word32

{-
Representation:
- each cell is two bits
- 00 f, 01 o, 10 x
- LSB is bit zero

0  1  2  3
4  5  6  7
8  9  10 11
12 13 14 15
-}

cell n board = shiftR board (shiftL n 1) .&. 0b11

setCell n letter board = shiftL letter (shiftL n 1) .|. board

-- https://www.baeldung.com/cs/generate-k-combinations#2-revolving-door-algorithm
grayCombos :: Int -> Int -> [Word16]
grayCombos _ 0 = [0]
grayCombos 0 _ = []
grayCombos n k = grayCombos (n - 1) k ++ ((.|. shiftL 1 (n - 1)) <$> reverse (grayCombos (n - 1) (k - 1)))

placeAll fPoss oPoss board = go fPoss oPoss 0 board
  where
    go _ _ 16 board = board
    go fPoss oPoss curPos board
      | fPoss .&. 1 == 1 =
        go (shiftR fPoss 1) oPoss (curPos + 1) (setCell curPos 0b00 board)
      | oPoss .&. 1 == 1 =
        go (shiftR fPoss 1) (shiftR oPoss 1) (curPos + 1) (setCell curPos 0b01 board)
      | otherwise = go (shiftR fPoss 1) (shiftR oPoss 1) (curPos + 1) (setCell curPos 0b10 board)

boardsGray :: [Board]
boardsGray = do
  fPoss <- grayCombos 16 fs
  oPoss <- grayCombos (16 - fs) os
  pure (placeAll fPoss oPoss 0)

foxPositions = forward ++ (reverse <$> forward)
  where
    forward =
      [ (0, 1, 2),
        (1, 2, 3),
        (4, 5, 6),
        (5, 6, 7),
        (8, 9, 10),
        (9, 10, 11),
        (12, 13, 14),
        (13, 14, 15),
        (0, 5, 10),
        (5, 10, 15),
        (1, 6, 11),
        (4, 9, 14),
        (0, 4, 8),
        (4, 8, 12),
        (1, 5, 9),
        (5, 9, 13),
        (2, 6, 10),
        (6, 10, 14),
        (3, 7, 11),
        (7, 11, 15),
        (2, 5, 8),
        (3, 6, 9),
        (6, 9, 12),
        (7, 10, 13)
      ]
    reverse (f, s, t) = (t, s, f)

hasFox board = getAny $ foldMap (\(f, s, t) -> Any $ cell f board == 0b00 && cell s board == 0b01 && cell t board == 0b10) foxPositions

showCell 0b00 = 'f'
showCell 0b01 = 'o'
showCell 0b10 = 'x'

showBoardRow row board = (\col -> showCell $ cell (col + row * 4) board) <$> [0, 1, 2, 3]

showBoard board = intercalate "\n" (flip showBoardRow board <$> [0, 1, 2, 3])

main = do
  print foxCombos
  print ofxCombos
  for_ (grayCombos 6 3) (putStrLn . flip (showIntAtBase 2 intToDigit) "")
  let wins = filter (not . hasFox) boardsGray
  print (length wins)
  for_ wins $ \b -> do
    putStrLn (showBoard b ++ "\n")
