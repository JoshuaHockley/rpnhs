module Inline (runInline) where

import Rpn (rpn, emptyState, Defs)
import RunLine (runSingle)
import Error

import Control.Monad.Except
import System.IO
import System.Exit
import qualified Data.Text as T


runInline :: Defs -> Bool -> Bool -> Bool -> String -> IO ()
runInline defs autoPrint ePrintProg ePrintStack l
  = let res = runExcept $ runSingle defs l
    in  case res of
          Right (out, (s, _)) -> success out s >> exitSuccess
          Left  e             -> failure e     >> exitFailure
  where
    success [] (v : _)
      | autoPrint = print v
    success out _ = mapM_ putStrLn out

    failure = mapM_ (hPutStrLn stderr) . showE
      where
        showE = showError ePrintProg ePrintStack (T.pack l)

