-- operators consume a number of Values as arguments
-- and return Nothing if the the operation fails, or the Values are of invalid types

module Operator (
  Operator1,
  Operator2,
  opAbs,
  opNegate,
  opRecip,
  opSqrt,
  opExp,
  opLn,
  opLog2,
  opAdd,
  opSubtract,
  opMultiply,
  opDivide,
  opIDivide,
  opMod,
  opPower,
  opLog,
  opSin,
  opCos,
  opTan,
  opSinh,
  opCosh,
  opTanh,
  opAsin,
  opAcos,
  opAtan,
  opAsinh,
  opAcosh,
  opAtanh,
  opNot,
  opAnd,
  opOr,
  opNand,
  opNor,
  opXor,
  opRnd,
  opFloor,
  opCeil,
  opFloat,
) where

import Value
import Util ((.:))

import Data.Function (on)
import Data.Ratio (numerator, denominator)
import Data.Bits

-- a unary operator
type Operator1 = Value -> Maybe Value

-- a binary operator
type Operator2 = Value -> Value -> Maybe Value

-- operator builders
--   build a generic operator from handlers for each type
--   will coerce values 'down' towards floats if needed
--   if no handler can be applied to the value(s), the opperator fails

type IntegerOp1  = Integer  -> Integer
type RationalOp1 = Rational -> Rational
type FloatingOp1 = Double   -> Double

buildOp1 :: Maybe IntegerOp1  -> Maybe RationalOp1 -> Maybe FloatingOp1 -> Operator1
buildOp1 (Just iop) _ _  (I i)  = Just . I       . iop                $ i
buildOp1 _ (Just rop) _  (I i)  = Just . reduceR . rop . fromIntegral $ i
buildOp1 _ (Just rop) _  (R r)  = Just . reduceR . rop                $ r
buildOp1 _ _ (Just fop)  v      = Just . F       . fop . asF          $ v
buildOp1 _ _ _           _      = Nothing  -- invalid type - fail

type IntegerOp2  = Integer  -> Integer  -> Integer
type RationalOp2 = Rational -> Rational -> Rational
type FloatingOp2 = Double   -> Double   -> Double

buildOp2 :: Maybe IntegerOp2  -> Maybe RationalOp2 -> Maybe FloatingOp2 -> Operator2
-- integer
buildOp2 (Just iop) _ _  (I i) (I i') = Just . I       $ iop i i'
-- rational
buildOp2 _ (Just rop) _  (I i) (I i') = Just . reduceR $ (rop `on` fromIntegral) i i'
buildOp2 _ (Just rop) _  (I i) (R r') = Just . reduceR $ rop (fromIntegral i) r'
buildOp2 _ (Just rop) _  (R r) (I i') = Just . reduceR $ rop r (fromIntegral i')
buildOp2 _ (Just rop) _  (R r) (R r') = Just . reduceR $ rop r r'
-- floating
buildOp2 _ _ (Just fop)  v     v'     = Just . F       $ (fop `on` asF) v v'
-- invalid types - fail
buildOp2 _ _ _           _     _      = Nothing


-- specialised op builders

buildIOp1 iop = buildOp1 (Just iop) Nothing Nothing
buildIOp2 iop = buildOp2 (Just iop) Nothing Nothing

buildFOp1 fop = buildOp1 Nothing Nothing (Just fop)
buildFOp2 fop = buildOp2 Nothing Nothing (Just fop)

buildOp1' iop rop fop = buildOp1 (Just iop) (Just rop) (Just fop)
buildOp2' iop rop fop = buildOp2 (Just iop) (Just rop) (Just fop)

buildRounder iop rop fop v
  = Just . I $ case v of I i -> iop i
                         R r -> rop r
                         F f -> fop f


-- operators

-- abs
opAbs = buildOp1' abs abs abs

-- negation
opNegate = Just . negateVal

-- reciprocol
opRecip = buildOp1 Nothing (Just recip) (Just recip)

-- square root
opSqrt v | isNeg v = Nothing
opSqrt v = buildFOp1 sqrt v

-- exp
opExp = buildFOp1 exp

-- natural log
opLn v | isNeg v || isZero v = Nothing
opLn v = buildFOp1 log v

-- log base 2
opLog2 = opLog (I 2)

-- addition
opAdd = buildOp2' (+) (+) (+)

-- subtraction
opSubtract = buildOp2' (-) (-) (-)

-- multiplication
opMultiply = buildOp2' (*) (*) (*)

-- real division
opDivide _ v | isZero v = Nothing
opDivide v v' = buildOp2 Nothing (Just (/)) (Just (/)) v v'

-- integer division
opIDivide _ v | isZero v = Nothing
opIDivide v v' = buildIOp2 div v v'

-- modulo
opMod _ v | isZero v = Nothing
opMod v v' = buildIOp2 mod v v'

-- power
opPower :: Operator2
opPower v (I i')  -- take advantage of an integer index
  | i' >= 0   = Just $ case v of I i -> I $ i ^ i'
                                 R r -> R $ r ^ i'
                                 F f -> F $ f ^ i'
  | otherwise = Just $ case v of I i -> R $ fromIntegral i ^^ i'
                                 R r -> R $ r ^^ i'
                                 F f -> F $ f ^^ i'
opPower v v' = buildFOp2 (**) v v'  -- use floating operation in other cases

-- log
opLog v _ | isNeg v || isZero v = Nothing
opLog _ v | isNeg v || isZero v = Nothing
opLog v v' = buildFOp2 (flip logBase) v v'  -- log_2(8) -> 8 2 log

-- trig
opSin   = buildFOp1 sin
opCos   = buildFOp1 cos
opTan   = buildFOp1 tan
opSinh  = buildFOp1 sinh
opCosh  = buildFOp1 cosh
opTanh  = buildFOp1 tanh
opAsin  = buildFOp1 asin
opAcos  = buildFOp1 acos
opAtan  = buildFOp1 atan
opAsinh = buildFOp1 asinh
opAcosh = buildFOp1 acosh
opAtanh = buildFOp1 atanh

-- bitwise
opNot  = buildIOp1 complement
opAnd  = buildIOp2 (.&.)
opOr   = buildIOp2 (.|.)
opNand = buildIOp2 (complement .: (.&.))
opNor  = buildIOp2 (complement .: (.|.))
opXor  = buildIOp2 xor

-- rounding
opRnd   = buildRounder id round   round
opFloor = buildRounder id floor   floor
opCeil  = buildRounder id ceiling ceiling

-- misc
opFloat = Just . F . asF
