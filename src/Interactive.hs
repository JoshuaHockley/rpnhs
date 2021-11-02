{-# LANGUAGE ViewPatterns #-}

module Interactive (runInteractive) where

import Rpn (rpn, State, emptyState)
import Parser
import Macros (Macros, expandMacros, parseMacro)
import Error

import System.Console.Haskeline
import Data.List


-- the context of the interactive mode (separate from the calculator)
type Context = (Macros, History)
-- history of calculator states
type History = [State]


runInteractive :: Macros -> IO ()
runInteractive ms = runInputT settings $ run (ms, []) emptyState
  where
    settings = Settings { complete = noCompletion,
                          historyFile = Nothing,
                          autoAddHistory = True }


run :: Context -> State -> InputT IO ()
run ctx s = do
  minput <- getInputLine "> "
  case minput of
    Just input -> handleInput ctx s input
    _          -> run ctx s


handleInput :: Context -> State -> String -> InputT IO ()
-- exiting
handleInput _ _ "q"  = return ()
handleInput _ _ ":q" = return ()
-- undo
handleInput (ms, h) s ":u"
  = case h of
      (s' : h') -> run (ms, h') s'
      _         -> run (ms, []) s
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
  = case rpn s =<< tokens of
      Ok (s', out) -> mapM_ putout out >> return (s', s : h)
      Err e        -> putoute e        >> return (s,      h)
  where
    tokens  = mapM parseToken . expandMacros ms $ words l
    putout  = (outputStr " " >>) . outputStrLn
    putoute = outputStrLn . show

