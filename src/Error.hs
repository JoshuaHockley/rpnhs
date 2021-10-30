module Error where

import Control.Monad


-- result type for rpnhs
data Result a = Ok  a
              | Err Error

toResult :: Error -> Maybe a -> Result a
-- convert a Maybe value to a result, with a provided error description
toResult _ (Just x) = Ok x
toResult e _        = Err e

assert :: (a -> Bool) -> Error -> a -> Result a
-- assert a predicate holds for a value
-- if the predicate does not hold, fail with the provided error
assert p e x
  | p x       = Ok  x
  | otherwise = Err e


-- error type
data Error = EmptyStackE               -- empty stack for a command that needs at least 1 value
           | PullE                     -- stack too small to perform the pull
           | UndefinedVarE String      -- attempt to load an uninitialised variable
           | PrintBaseNonIntegerE      -- tried to print a non-integer value in a custom base
           | OperatorFailureE          -- operator failed (e.g. invalid types, not enough opperands)

           | TokenParseE String        -- failed to parse a string as a token
           | FracParseE String         -- fraction literal contained an invalid componant e.g. 3/hi
           | InvalidBaseSE String      -- base not an integer
           | InvalidBaseE Int          -- invalid base to print/read an integer
           | InvalidDigitE Int Char    -- invalid digit under a base


-- user facing error description
instance Show Error where
  show EmptyStackE          = "error: empty stack"
  show PullE                = "error: stack was too small to perform the pull"
  show (UndefinedVarE s)    = "error: variable is undefined (" ++ s ++ ")"
  show PrintBaseNonIntegerE = "error: cannot print non-integer values in custom bases"
  show OperatorFailureE     = "error: operator failed"

  show (TokenParseE s)      = "parse error: unrecognised token (" ++ s ++ ")"
  show (FracParseE s)       = "parse error: invalid fraction componant (" ++ s ++ ")"
  show (InvalidBaseSE s)    = "base error: invalid base (" ++ ")"
  show (InvalidBaseE i)     = "base error: invalid base (" ++ show i ++ ")"
  show (InvalidDigitE b c)  = "base error: " ++ c : " is an invalid digit under base " ++ show b


-- Result behaves like Maybe as a monad
-- in Err cases, the Result is unmodified
instance Monad Result where
  return = Ok

  Ok  x >>= f = f x
  Err e >>= f = Err e

instance Applicative Result where
  pure = return

  (<*>) = ap

instance Functor Result where
  fmap f = (=<<) (return . f)


