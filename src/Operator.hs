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
  opFact,
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
  opDeg,
  opRad,
  opNot,
  opAnd,
  opOr,
  opNand,
  opNor,
  opXor,
  opLShift,
  opRShift,
  opLt,
  opLte,
  opEq,
  opNeq,
  opGte,
  opGt,
  opRnd,
  opFloor,
  opCeil,
  opFloat,
) where

import Value
import Util ((.:))

import Data.Function (on)
import Data.Ratio (numerator, denominator)
import Data.Fixed
import Data.Bits

-- a unary operator
type Operator1 = Value -> Maybe Value

-- a binary operator
type Operator2 = Value -> Value -> Maybe Value

-- operator builders
--   build a generic operator from handlers for each type
--   will coerce values 'down' towards floats if needed
--   if no handler can be applied to the value(s), the opperator fails

type IntegerOp1  = Integer  -> Value
type RationalOp1 = Rational -> Value
type FloatingOp1 = Double   -> Value

buildOp1 :: Maybe IntegerOp1  -> Maybe RationalOp1 -> Maybe FloatingOp1 -> Operator1
buildOp1 (Just iop) _ _  (I i)  = Just . iop                $ i
buildOp1 _ (Just rop) _  (I i)  = Just . rop . fromIntegral $ i
buildOp1 _ (Just rop) _  (R r)  = Just . rop                $ r
buildOp1 _ _ (Just fop)  v      = Just . fop . asF          $ v
buildOp1 _ _ _           _      = Nothing  -- invalid type - fail

type IntegerOp2  = Integer  -> Integer  -> Value
type RationalOp2 = Rational -> Rational -> Value
type FloatingOp2 = Double   -> Double   -> Value

buildOp2 :: Maybe IntegerOp2  -> Maybe RationalOp2 -> Maybe FloatingOp2 -> Operator2
-- integer
buildOp2 (Just iop) _ _  (I i) (I i') = Just $ iop i i'
-- rational
buildOp2 _ (Just rop) _  (I i) (I i') = Just $ (rop `on` fromIntegral) i i'
buildOp2 _ (Just rop) _  (I i) (R r') = Just $ rop (fromIntegral i) r'
buildOp2 _ (Just rop) _  (R r) (I i') = Just $ rop r (fromIntegral i')
buildOp2 _ (Just rop) _  (R r) (R r') = Just $ rop r r'
-- floating
buildOp2 _ _ (Just fop)  v     v'     = Just $ (fop `on` asF) v v'
-- invalid types - fail
buildOp2 _ _ _           _     _      = Nothing


-- specialised op builders

-- build an operator that remains in the type of it's operands
buildOp1' iop rop fop = buildOp1 (fmap (I . ) iop) (fmap (reduceR . ) rop) (fmap (F . ) fop)
buildOp2' iop rop fop = buildOp2 (fmap (I .:) iop) (fmap (reduceR .:) rop) (fmap (F .:) fop)

-- build a total operator (handler for all types)
buildTOp1  iop rop fop = buildOp1  (Just iop) (Just rop) (Just fop)
buildTOp2  iop rop fop = buildOp2  (Just iop) (Just rop) (Just fop)
buildTOp1' iop rop fop = buildOp1' (Just iop) (Just rop) (Just fop)
buildTOp2' iop rop fop = buildOp2' (Just iop) (Just rop) (Just fop)

buildIOp1' iop = buildOp1' (Just iop) Nothing Nothing
buildIOp2' iop = buildOp2' (Just iop) Nothing Nothing

buildFOp1' fop = buildOp1' Nothing Nothing (Just fop)
buildFOp2' fop = buildOp2' Nothing Nothing (Just fop)

buildCmp iop rop fop = buildTOp2 (fromEnum' .: iop) (fromEnum' .: rop) (fromEnum' .: fop)
  where fromEnum' = I . toInteger . fromEnum

buildRounder rop fop = buildTOp1 I (I . rop) (I . fop)


-- operators

-- abs
opAbs = buildTOp1' abs abs abs

-- negation
opNegate = Just . negateVal

-- reciprocol
opRecip = buildOp1' Nothing (Just recip) (Just recip)

-- square root
opSqrt v | isNeg v = Nothing
opSqrt v = buildFOp1' sqrt v

-- exp
opExp = buildFOp1' exp

-- natural log
opLn v | isNeg v || isZero v = Nothing
opLn v = buildFOp1' log v

-- log base 2
opLog2 = opLog (I 2)

-- factorial
opFact v | isNeg v = Nothing
opFact v = buildIOp1' fact v
  where
    fact i = product [1..i]

-- addition
opAdd = buildTOp2' (+) (+) (+)

-- subtraction
opSubtract = buildTOp2' (-) (-) (-)

-- multiplication
opMultiply = buildTOp2' (*) (*) (*)

-- real division
opDivide _ v | isZero v = Nothing
opDivide v v' = buildOp2' Nothing (Just (/)) (Just (/)) v v'

-- integer division
opIDivide _ v | isZero v = Nothing
opIDivide v v' = buildTOp2 (I .: div) (I .: div') (I .: div') v v'

-- modulo
opMod _ v | isZero v = Nothing
opMod v v' = buildOp2' (Just mod) Nothing (Just mod') v v'

-- power
opPower :: Operator2
opPower v (I i')  -- take advantage of an integer index
  | i' >= 0   = Just $ case v of I i -> I $ i ^ i'
                                 R r -> R $ r ^ i'
                                 F f -> F $ f ^ i'
  | otherwise = Just $ case v of I i -> R $ fromIntegral i ^^ i'
                                 R r -> R $ r ^^ i'
                                 F f -> F $ f ^^ i'
opPower v v' = buildFOp2' (**) v v'  -- use floating operation in other cases

-- log
opLog v _ | isNeg v || isZero v = Nothing
opLog _ v | isNeg v || isZero v = Nothing
opLog v v' = buildFOp2' (flip logBase) v v'  -- log_2(8) -> 8 2 log

-- trig
opSin   = buildFOp1' sin
opCos   = buildFOp1' cos
opTan   = buildFOp1' tan
opSinh  = buildFOp1' sinh
opCosh  = buildFOp1' cosh
opTanh  = buildFOp1' tanh
opAsin  = buildFOp1' asin
opAcos  = buildFOp1' acos
opAtan  = buildFOp1' atan
opAsinh = buildFOp1' asinh
opAcosh = buildFOp1' acosh
opAtanh = buildFOp1' atanh

opDeg = buildFOp1' ((`mod'` 360     ) . (* (180 / pi)))
opRad = buildFOp1' ((`mod'` (2 * pi)) . (* (pi / 180)))

-- bitwise
opNot  = buildIOp1' complement
opAnd  = buildIOp2' (.&.)
opOr   = buildIOp2' (.|.)
opNand = buildIOp2' (complement .: (.&.))
opNor  = buildIOp2' (complement .: (.|.))
opXor  = buildIOp2' xor

-- shifts
opLShift = buildIOp2' (\i i' -> shift i (fromInteger i'))
opRShift v v' = opLShift v (negateVal v')

-- comparisons
opLt  = buildCmp (<)  (<)  (<)
opLte = buildCmp (<=) (<=) (<=)
opEq  = buildCmp (==) (==) (==)
opNeq = buildCmp (/=) (/=) (/=)
opGte = buildCmp (>=) (>=) (>=)
opGt  = buildCmp (>)  (>)  (>)

-- misc
opRnd   = buildRounder round   round
opFloor = buildRounder floor   floor
opCeil  = buildRounder ceiling ceiling
opFloat = Just . F . asF
