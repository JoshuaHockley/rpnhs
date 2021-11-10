module Inline (runInline) where

import Rpn (rpn, emptyState)
import Parser
import Macros (Macros, expandMacros)
import Error

import System.IO
import System.Exit


runInline :: Macros -> Bool -> [String] -> IO ()
runInline ms autoPrint args = do
  case rpn emptyState =<< tokens of
    Ok ((s, _), out) -> success s out   >> exitSuccess
    Err e            -> hPrint stderr e >> exitFailure
  where
    tokens = mapM parseToken . expandMacros ms $ concatMap words args
    success (v : _) []
      | autoPrint = print v
    success _ out = mapM_ putStrLn out
