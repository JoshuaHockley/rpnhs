module Inline (runInline) where

import Rpn (rpn, emptyState)
import LineProcessor
import Macros (Macros)
import Error

import System.IO
import System.Exit


runInline :: Macros -> Bool -> Bool -> Bool -> [String] -> IO ()
runInline ms autoPrint ePrintInstr ePrintStack args
  = case res of
      Right ((s, _), out) -> success s out >> exitSuccess
      Left  e             -> failure e     >> exitFailure
  where
    res = do
      is <- processLine ms (concatMap words args)
      rpn emptyState is

    success (v : _) []
      | autoPrint = print v
    success _ out = mapM_ putStrLn out

    failure = mapM_ (hPutStrLn stderr) . showE'

    showE' = showE ePrintInstr ePrintStack

