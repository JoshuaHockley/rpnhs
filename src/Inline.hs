module Inline (runInline) where

import Rpn (rpn, emptyState)
import LineProcessor
import Macros (Macros)
import Error

import System.IO
import System.Exit


runInline :: Macros -> Bool -> [String] -> IO ()
runInline ms autoPrint args = do
  case res of
    Ok ((s, _), out) -> success s out   >> exitSuccess
    Err e            -> hPrint stderr e >> exitFailure
  where
    res = do
      (tokens, jtable) <- processLine ms (concatMap words args)
      rpn jtable emptyState tokens
    success (v : _) []
      | autoPrint = print v
    success _ out = mapM_ putStrLn out
