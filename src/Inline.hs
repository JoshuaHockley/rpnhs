module Inline (runInline) where

import Rpn (rpn, emptyState)
import LineProcessor
import Macros (Macros)
import Error

import System.IO
import System.Exit
import qualified Data.Text as T


runInline :: Macros -> Bool -> Bool -> Bool -> [String] -> IO ()
runInline ms autoPrint ePrintProg ePrintStack args
  = case res of
      Right ((s, _), out) -> success s out >> exitSuccess
      Left  e             -> failure e     >> exitFailure
  where
    l = unwords args

    res = do
      is <- mapErr ParseE $ processLine ms l
      mapErr CalcE $ rpn emptyState is

    success (v : _) []
      | autoPrint = print v
    success _ out = mapM_ putStrLn out

    failure = mapM_ (hPutStrLn stderr) . showE'

    showE' = showE ePrintProg ePrintStack (T.pack l)

