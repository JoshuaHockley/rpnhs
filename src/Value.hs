module Value where

import Data.Ratio
import Data.Char (isSpace)

-- the stack of values to process
type Stack = [Value]

-- a numeric value that can be operated on
data Value = I Integer   -- integral value
           | R Rational  -- ratio value
           | F Double    -- floating value


-- utils

asI :: Value -> Maybe Integer
asI (I i) = Just i
asI _     = Nothing

onValue fn _ _ (I i) = fn i
onValue _ fn _ (R r) = fn r
onValue _ _ fn (F f) = fn f

asF :: Value -> Double
asF = onValue fromInteger fromRational id

isZero = onValue (== 0) (== 0) (== 0)

isNeg = onValue (< 0) (< 0) (< 0)

negateVal = onValue (I . negate) (R . negate) (F . negate)

-- rational reduction
--   when a result is rational, we check if the denominator is 1
--   in this case we can reduce to integer value
--   e.g. 10 2 /    => 5  (not 5 % 1)
--        5/4 1/4 - => 1  (not 1 % 1)
--   this function is used in place of 'R', when a reduction may be possible
reduceR :: Rational -> Value
reduceR r
  | denominator r == 1 = I $ numerator r
  | otherwise          = R r

validateValue :: Value -> Maybe Value
validateValue (F f) | isNaN f = Nothing
validateValue v               = Just v


-- showing

instance Show Value where
  show (I i) = show i
  show (R r) = show (numerator r) ++ "/" ++ show (denominator r)
  show (F f) = show f

showStack :: Stack -> Maybe String
-- show the stack
-- reversed so operands are shown in the correct order: "... 2 3" -> 2 op 3
showStack [] = Nothing
showStack vs = Just . trim . concatMap ((' ' :) . show) $ reverse vs
  where trim = dropWhile isSpace
