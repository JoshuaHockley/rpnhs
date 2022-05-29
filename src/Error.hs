{-# LANGUAGE TupleSections #-}

module Error where

import Value

import Control.Monad.Except
import Text.Megaparsec.Error (ParseErrorBundle, ShowErrorComponent, showErrorComponent, errorBundlePretty)
import Data.Maybe
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T


data Error  = CalcE  CtxError
            | ParseE ParseError

type CtxError = (CalcError, ErrorCtx)

-- context of the calculator before an error
-- (position in program, stack)
type ErrorCtx = (Int, Stack)

data CalcError = EmptyStackE           -- empty stack for an operator or command
               | PullE                 -- stack too small to perform the pull
               | PushE                 -- stack too small to perform the push
               | UndefinedVarE String  -- attempt to load an uninitialised variable
               | PrintBaseNonIntegerE  -- tried to print a non-integer value in a custom base
               | OperatorFailureE      -- operator failed (e.g. invalid types)

type ParseError = ParseErrorBundle Text LogicParseError

data LogicParseError = ZeroIntArg              -- 0 as an integer argument to a command
                     | InvalidBaseE Int        -- invalid base to print/read an integer
                     | InvalidDigitE Int Char  -- invalid digit under a base
  deriving (Eq, Show, Ord)


-- utils

unwrap :: MonadError e m => e -> Maybe a -> m a
unwrap e = maybe (throwError e) return

assert :: MonadError e m => e -> Bool -> m ()
assert e False = throwError e
assert _ _     = return ()

withContext :: ErrorCtx -> CalcError -> CtxError
withContext ctx = (, ctx)


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
  = showParseError err

instance Show CalcError where
  show EmptyStackE          = "empty stack"
  show PullE                = "stack was too small to perform the pull"
  show PushE                = "stack was too small to perform the push"
  show (UndefinedVarE s)    = "variable is undefined (" ++ s ++ ")"
  show PrintBaseNonIntegerE = "cannot print non-integer values in bases other than decimal"
  show OperatorFailureE     = "operator failed"

showParseError :: ParseError -> [String]
showParseError e = lines (errorBundlePretty e) ++ [""]

instance ShowErrorComponent LogicParseError where
  showErrorComponent ZeroIntArg          = "integer argument cannot be 0"
  showErrorComponent (InvalidBaseE n)    = "invalid base `" ++ show n ++ "`"
  showErrorComponent (InvalidDigitE n c) = "invalid digit `" ++ [c] ++ "` in base " ++ show n

