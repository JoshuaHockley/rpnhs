module Bases (showB, parseB, validBase) where

import Error

import Data.List (foldl', elemIndex)
import Data.Array
import Control.Monad (mfilter)


digits :: Array Int Char
-- available symbols for base representations
digits = listArray (0, length digits' - 1) digits'
  where digits' = ['0'..'9'] ++ ['a'..'z']

-- maximum base for printing/parsing
maxBase = length digits

validBase b = b >= 2 && b <= maxBase


parseB :: Bool -> Int -> String -> LogicParseResult Integer
-- parse an integer representation in an arbitrary base
-- pre: string is non-empty
parseB compl b s = (if compl then fromRadixComp b (length s) else id)
                   .   compose b
                   <$> digitValues b s
  where
    digitValues :: Int -> String -> LogicParseResult [Int]
    digitValues b s  = mapM digitValue s
      where
        digitValue :: Char -> LogicParseResult Int
        digitValue c = maybe (Left (InvalidDigitE b c)) pure $ mfilter (< b) (elemIndex c (elems digits))


showB :: Bool -> Int -> Integer -> String
-- string representation of an integer in an arbitrary base [2, maxBase]
-- negatives are represented with a minus sign or the radix complement
--   depending on the value of compl
-- pre: 2 <= b <= maxBase
showB compl b i
  | i >= 0    = showB' . (if compl then handlePositiveComp else decomp b) $ i
  | compl     = showB' . decomp b $ intoRadixComp b i
  | otherwise = ('-' :) . showB' $ decomp b (abs i)
  where
    showB' :: [Int] -> String
    showB' [] = [digits ! 0]
    showB' xs = map (digits !) xs  -- values < maxBase

    handlePositiveComp :: Integer -> [Int]
    handlePositiveComp i
      | fromComp < 0 = 0 : vs  -- prepend 0 to disambiguate positives e.g. 1010 -> 01010
      | otherwise    =     vs
      where
        vs       = decomp b i
        fromComp = fromRadixComp b (length vs) i


compose :: Int -> [Int] -> Integer
-- compute the unsigned value represented by the list of digits in base b
-- [1, 1, 0, 1] -> 13  (b=2)
-- [1, 3]       -> 13  (b=10)
compose b = foldl' (\acc v -> acc * toInteger b + toInteger v) 0


decomp :: Int -> Integer -> [Int]
-- digit values of a positive integer in base b
-- 13 -> [1, 1, 0, 1]  (b=2)
-- 13 -> [1, 3]        (b=10)
-- 0  -> [0]           (always)
decomp b = reverse . decomp'
  where
    decomp' 0 = []
    decomp' i = fromInteger r : decomp' q
      where
        (q, r) = quotRem i (toInteger b)


fromRadixComp :: Int -> Int -> Integer -> Integer
-- convert a value in a radix complement to its true value, based on the number of digits provided
-- e.g. 11 [1011]  -> -5 [-0101]  (b=2, digits=4)
--      11 [01011] -> 11 [ 1011]  (b=2, digits=5)
fromRadixComp b digits i
  | isNegative = i - (minus1 + 1)  -- compute true value
  | otherwise  = i                 -- positive values do not need changing
  where
    -- value of -1 in the radix complement defined by the number of digits
    minus1 = toInteger b ^ digits - 1
    -- the sign of i's true value
    isNegative = i > minus1 `div` 2


intoRadixComp :: Int -> Integer -> Integer
-- convert a true value to its radix complement form, with the fewest digits possible
-- e.g. -5 [-0101] -> 11 [1011]  (b=2)
--      5  [ 0101] -> 5  [101]   (b=2)
intoRadixComp b i
  | i < 0     = minus1 + 1 + i  -- compute radix complement form
  | otherwise = i               -- positive values do not need changing
  where
    -- value of -1 in our chosen radix complement
    -- e.g. where b=2, i=-5 -> minus1 = 15 [1111]
    minus1 | abs i > nextPower `div` 2 = b' * nextPower - 1
           | otherwise                 =      nextPower - 1
      where nextPower = b' ^ ceiling (logBase b'' (abs i'))
            b'  = fromIntegral b
            b'' = fromIntegral b
            i'  = fromIntegral i

