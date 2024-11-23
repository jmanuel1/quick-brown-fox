{-# LANGUAGE BinaryLiterals #-}

import Data.Bits
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Data.Word

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

-- QUESTION: Can I generate boards just from an integer?
combos :: Int -> [a] -> [[a]]
combos 0 options = [[]]
combos k options =
  concatMap go (tails options) where
    go :: [a] -> [[a]]
    go os =
      case os of
        (o : os) -> (o :) <$> combos (k - 1) os
        [] -> []

positions = [0 .. 15]

placeFs poss board = do
  poss <- combos fs poss
  pure $ (poss, foldr (\p b -> setCell p 0b00 b) board poss)

placeOs poss board = do
  poss <- combos os poss
  pure $ (poss, foldr (\p b -> setCell p 0b01 b) board poss)

placeXs poss board = do
  poss <- combos xs poss
  pure $ foldr (\p b -> setCell p 0b10 b) board poss

boards :: [Board]
boards = do
  (used, b) <- placeFs positions 0
  let positions' = filter (not . (`elem` used)) positions
  (used, b) <- placeOs positions' b
  let positions'' = filter (not . (`elem` used)) positions'
  placeXs positions'' b

foxPositions = forward ++ (reverse <$> forward) where
  forward =
    [[0, 1, 2],
      [1, 2, 3],
      [4, 5, 6],
      [5, 6, 7],
      [8, 9, 10],
      [9, 10, 11],
      [12, 13, 14],
      [13, 14, 15],
      [0, 5, 10],
      [5, 10, 15],
      [1, 6, 11],
      [4, 9, 14],
      [0, 4, 8],
      [4, 8, 12],
      [1, 5, 9],
      [5, 9, 13],
      [2, 6, 10],
      [6, 10, 14],
      [3, 7, 11],
      [7, 11, 15],
      [2, 5, 8],
      [3, 6, 9],
      [6, 9, 12],
      [7, 10, 13]]

hasFox board = getAny $ foldMap (\ps -> Any $ cell (ps !! 0) board == 0b00 && cell (ps !! 1) board == 0b01 && cell (ps !! 2) board == 0b10) foxPositions

main = do
  print foxCombos
  print ofxCombos
  print (combos 1 [1 .. 16])
  print (combos 2 [1 .. 16])
  print (length (combos fs [1 .. 16]) * length (combos os [1 .. 16 - fs]))
  print (length $ filter (not . hasFox) boards)
