{-# LANGUAGE TupleSections #-}

module Error where

import Value

import Text.Megaparsec.Error (ParseErrorBundle, ShowErrorComponent, showErrorComponent, errorBundlePretty)
import Data.Maybe
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T


type Result = Either Error

data Error  = CalcE  CtxError
            | ParseE ParseError

type CtxResult = Either CtxError

type CtxError = (CalcError, ErrorCtx)

-- context of the calculator before an error
-- (position in program, stack)
type ErrorCtx = (Int, Stack)

type CalcResult = Either CalcError

data CalcError = EmptyStackE           -- empty stack for a command that needs at least 1 value
               | PullE                 -- stack too small to perform the pull
               | PushE                 -- stack too small to perform the push
               | UndefinedVarE String  -- attempt to load an uninitialised variable
               | PrintBaseNonIntegerE  -- tried to print a non-integer value in a custom base
               | OperatorFailureE      -- operator failed (e.g. invalid types)
               | NotEnoughOperandsE    -- not enough operands to apply an operator

type ParseResult = Either ParseError

type ParseError = ParseErrorBundle Text LogicParseError

type LogicParseResult = Either LogicParseError

data LogicParseError = ZeroIntArg              -- 0 as an integer argument to a command
                     | InvalidBaseE Int        -- invalid base to print/read an integer
                     | InvalidDigitE Int Char  -- invalid digit under a base
  deriving (Eq, Show, Ord)


-- utils

mapErr :: (e -> e') -> Either e a -> Either e' a
mapErr = first

toResult :: e -> Maybe a -> Either e a
-- convert a Maybe value to a Result, with a provided error description
toResult e = maybe (Left e) return

assert :: Bool -> e -> Either e ()
-- assert a predicate holds for a value
-- if the predicate does not hold, fail with the provided error
assert False e = Left e
assert _     _ = return ()

withContext :: ErrorCtx -> CalcResult a -> CtxResult a
withContext ctx = first (, ctx)


-- display

showE :: Bool -> Bool -> Text -> Error -> [String]
-- user facing output of an error
-- context printing is based on settings
-- print instructions -> print stack -> ...
showE printProg printStack prog (CalcE (err, (pos, stack)))
  = ("error: " ++ show err)
    :  (if printProg
           then [" program   " ++ T.unpack prog,
                 "           " ++ replicate pos ' ' ++ "^"]
           else [])
    ++ [" stack     " ++ fromMaybe "[]" (showStack stack) | printStack]
    ++ [""]
showE _ _ _ (ParseE err)
  = lines (errorBundlePretty err) ++ [""]

instance Show CalcError where
  show EmptyStackE          = "empty stack"
  show PullE                = "stack was too small to perform the pull"
  show PushE                = "stack was too small to perform the push"
  show (UndefinedVarE s)    = "variable is undefined (" ++ s ++ ")"
  show PrintBaseNonIntegerE = "cannot print non-integer values in bases other than decimal"
  show OperatorFailureE     = "operator failed"
  show NotEnoughOperandsE   = "not enough operands to apply the operator"

instance ShowErrorComponent LogicParseError where
  showErrorComponent ZeroIntArg          = "integer argument cannot be 0"
  showErrorComponent (InvalidBaseE n)    = "invalid base `" ++ show n ++ "`"
  showErrorComponent (InvalidDigitE n c) = "invalid digit `" ++ [c] ++ "` in base " ++ show n

