module Main where

import Rpn (rpn, State, emptyState)
import Parser
import Error

import Control.Monad
import Data.List (words)
import System.IO
import System.Exit
import System.Environment (getArgs)
import System.Console.Haskeline


main :: IO ()
main = do args <- getArgs
          case args of
            [] -> runInteractive  -- no args - run interactive mode
            _  -> runInline args  -- args    - run inline mode


-- inline mode

runInline :: [String] -> IO ()
runInline args = case rpn emptyState =<< tokens of
                   Ok (_, out) -> mapM_ putStrLn out >> exitSuccess
                   Err e       -> hPrint stderr e    >> exitFailure
  where
    tokens = mapM parseToken (concatMap words args)


-- interactive mode

runInteractive :: IO ()
runInteractive = runInputT settings (run emptyState)
  where
    run :: State -> InputT IO ()
    run s = do
      minput <- getInputLine "> "
      case minput of
        Nothing    -> run s
        Just "q"   -> return ()
        Just input -> run =<< runLine s input

    runLine :: State -> String -> InputT IO State
    runLine s l = case rpn s =<< tokens of
                       Ok (s', out) -> mapM_ putout out >> return s'
                       Err e        -> putoute e        >> return s
      where
        tokens  = mapM parseToken (words l)
        putout  = (outputStr " " >>) . outputStrLn
        putoute = outputStrLn . show

    settings = Settings { complete = noCompletion,
                          historyFile = Nothing,
                          autoAddHistory = True }
