{-# LANGUAGE ViewPatterns #-}

module Interactive (runInteractive) where

import Rpn (rpn, State, emptyState)
import LineProcessor
import Macros (Macros, parseMacro)
import Error

import System.Console.Haskeline
import Control.Monad (mfilter)
import Data.List (stripPrefix, isPrefixOf)
import Data.Maybe
import Text.Read (readMaybe)


-- the context of the interactive mode (separate from the calculator)
type Context = (Macros, History)
-- history of calculator states
type History = [State]


runInteractive :: Macros -> String -> Bool -> Bool -> IO ()
runInteractive ms prompt ePrintInstr ePrintStack
  = runInputT settings $ run (ms, []) emptyState
  where
    settings = Settings { complete = noCompletion,
                          historyFile = Nothing,
                          autoAddHistory = True }


    run :: Context -> State -> InputT IO ()
    run ctx s = do
      minput <- getInputLine prompt
      case minput of
        Just input -> handleInput ctx s input
        _          -> run ctx s


    handleInput :: Context -> State -> String -> InputT IO ()
    -- exiting
    handleInput _ _ "q"  = return ()
    handleInput _ _ ":q" = return ()
    -- undo
    handleInput (ms, h) s (stripPrefix ":u" -> Just n)
      = run (ms, h') s'
      where
        (s', h') = maybe (s, h) undo n'
        n'       = if null n then Just 1 else mfilter (>= 1) (readMaybe n)
        undo :: Int -> (State, History)
        undo n = case drop (n - 1) h of
                   (s' : h') -> (s', h')
                   _         -> (emptyState, [])
    -- macro definition
    handleInput (ms, h) s (stripPrefix ":def " -> Just m)
      = case parseMacro m of
          Just m' -> run (m' : ms, h) s
          _       -> outputStrLn "error: failed to parse macro" >> run (ms, h) s
    -- run the calculator on the line
    handleInput (ms, h) s input = do
      (s', h') <- runLine (ms, h) s input
      run (ms, h') s'


    runLine :: Context -> State -> String -> InputT IO (State, History)
    -- run the calculator on a line of input
    -- return the next state and updated history (both unchanged on error)
    runLine (ms, h) s l
      = case res of
          Right (s', out) -> mapM_ putout out >> return (s', s : h)
          Left  e         -> putoute e        >> return (s,      h)
      where
        res = do
          (tokens, jtable) <- processLine ms (words l)
          rpn jtable s tokens
        putout       = (outputStr " " >>) . outputStrLn
        putoute      = mapM_ outputStrLn . showE'

    showE' = showE ePrintInstr ePrintStack

