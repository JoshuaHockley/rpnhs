module Inline (runInline) where

import Rpn (rpn, emptyState)
import Parser
import Macros (Macros, expandMacros)
import Error

import System.IO
import System.Exit


runInline :: Macros -> [String] -> IO ()
runInline ms args = do
  case rpn emptyState =<< tokens of
    Ok (_, out) -> mapM_ putStrLn out >> exitSuccess
    Err e       -> hPrint stderr e    >> exitFailure
  where
    tokens = mapM parseToken . expandMacros ms $ concatMap words args
