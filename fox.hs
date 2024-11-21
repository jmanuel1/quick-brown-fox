{-# LANGUAGE BinaryLiterals #-}

import Data.Bits
import Data.List
import Data.Maybe
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
      case listToMaybe os of
        Just o ->
          (o :) <$> combos (k - 1) (tail os)
        Nothing -> []

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

-- count letter board = foldMap (\i => cell i board == letter)

main = do
  print foxCombos
  print ofxCombos
  print (combos 1 [1 .. 16])
  print (combos 2 [1 .. 16])
  print (length (combos fs [1 .. 16]) * length (combos os [1 .. 16 - fs]))
  print (length boards)
