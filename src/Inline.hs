module Inline (runInline) where

import Rpn (rpn, emptyState)
import RunLine (runSingle)
import Macros (Macros)
import Error

import Control.Monad.Except
import System.IO
import System.Exit
import qualified Data.Text as T


runInline :: Macros -> Bool -> Bool -> Bool -> String -> IO ()
runInline ms autoPrint ePrintProg ePrintStack l
  = let res = runExcept $ runSingle ms l
    in  case res of
          Right (out, (s, _)) -> success out s >> exitSuccess
          Left  e             -> failure e     >> exitFailure
  where
    success [] (v : _)
      | autoPrint = print v
    success out _ = mapM_ putStrLn out

    failure = mapM_ (hPutStrLn stderr) . showE'
      where
        showE' = showE ePrintProg ePrintStack (T.pack l)

