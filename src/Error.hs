module Error where

import Value

import Control.Monad
import Data.Maybe


-- result type for rpnhs
data Result a = Ok  a
              | Err Error

mkErr :: ErrDesc -> Result a
-- make a contextless erroroneous Result
mkErr desc = Err $ Error desc Nothing

toResult :: ErrDesc -> Maybe a -> Result a
-- convert a Maybe value to a Result, with a provided error description
toResult _ (Just x) = Ok x
toResult e _        = mkErr e

assert :: Bool -> ErrDesc -> Result ()
-- assert a predicate holds for a value
-- if the predicate does not hold, fail with the provided error
assert False e = mkErr e
assert _     _ = Ok ()

withContext :: EContext -> Result a -> Result a
-- inject a context into a Result
withContext ctx (Err (Error desc _)) = Err (Error desc (Just ctx))
withContext _ r = r


-- error type
-- contains the description and the context it occured in (Nothing when not calculator related)
data Error = Error ErrDesc (Maybe EContext)

-- context of the calculator before an error
type EContext = ([String], Stack)

data ErrDesc = EmptyStackE               -- empty stack for a command that needs at least 1 value
             | PullE                     -- stack too small to perform the pull
             | UndefinedVarE String      -- attempt to load an uninitialised variable
             | PrintBaseNonIntegerE      -- tried to print a non-integer value in a custom base
             | OperatorFailureE          -- operator failed (e.g. invalid types, not enough opperands)

             | TokenParseE String        -- failed to parse a string as a token
             | FracParseE String         -- fraction literal contained an invalid componant e.g. 3/hi
             | InvalidBaseSE String      -- base not an integer
             | InvalidBaseE Int          -- invalid base to print/read an integer
             | InvalidDigitE Int Char    -- invalid digit under a base
             | EmptyBaseLiteralE         -- no digits given in a base literal
              
             | UndefinedLabelE String    -- attempt to jump to an undefined label
             | DuplicateLabelE String    -- label defined multiple times

             | UserErrorE                -- user error triggered by the ERR command


showE :: Bool -> Bool -> Error -> [String]
-- user facing output of an error
-- context printing is based on settings
-- print instructions -> print stack -> ...
showE printInstr printStack (Error desc (Just (instr, stack)))
  = show desc
    :  [" at     " ++ unwords instr                    | printInstr]
    ++ [" with   " ++ fromMaybe "[]" (showStack stack) | printStack]
showE _ _ (Error desc _)
  = [show desc]

-- user facing error description
instance Show ErrDesc where
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
  show EmptyBaseLiteralE    = "base error: empty base literal"

  show (UndefinedLabelE l)  = "jump error: label " ++ l ++ " is undefined"
  show (DuplicateLabelE l)  = "label error: label " ++ l ++ " is defined multiple times"

  show UserErrorE           = "user error"


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

