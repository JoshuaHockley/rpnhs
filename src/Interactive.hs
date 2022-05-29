{-# LANGUAGE ViewPatterns #-}

module Interactive (runInteractive) where

import Rpn (rpn, CalcState, emptyState, Defs)
import RunLine (runLine)
import Subroutine
import Error

import Control.Monad (mfilter)
import Control.Monad.State
import Control.Monad.Except
import System.Console.Haskeline
import Data.List (stripPrefix, isPrefixOf)
import Data.Maybe
import qualified Data.Text as T
import Text.Read (readMaybe)


-- monad stack for the interactive calculator
type ICalc = StateT ICalcState (InputT IO)

-- the state of the interactive calculator
type ICalcState = (CalcState, Context)

-- the context of the interactive mode (separate from the calculator)
type Context = (Defs, History)
-- history of calculator states
type History = [CalcState]


runInteractive :: Defs -> String -> Bool -> Bool -> IO ()
runInteractive defs prompt ePrintProg ePrintStack
  = void
    . runInputT settings
    . flip runStateT initialState
    $ run' prompt ePrintProg ePrintStack
  where
    settings = Settings { complete = noCompletion,
                          historyFile = Nothing,
                          autoAddHistory = True }
    initialState = (emptyState, (defs, [])) :: ICalcState


run' :: String -> Bool -> Bool -> ICalc ()
run' prompt ePrintProg ePrintStack = run
  where

    run :: ICalc ()
    run = do
      minput <- lift $ getInputLine prompt
      case minput of
        Just input -> case input of
          -- quit
          ":q" -> return ()
          "q"  -> return ()
          -- execute input
          _    -> handleInput input >> run
        -- ignore empty line
        _ -> run


    handleInput :: String -> ICalc ()
    handleInput s = case stripPrefix ":" s of
      Just c -> handleMetaCommand c
      _      -> runCalc s


    handleMetaCommand s = case s of
      -- undo
      (stripPrefix "u" -> Just s) ->
        case mn of
          Just n -> undo n
          _      -> lift $ outputStrLn "error: invalid undo level"
        where
          mn | null s    = Just 1  -- interpret `:u` as `:u1`
             | otherwise = mfilter (>= 1) (readMaybe s) :: Maybe Int

      -- def
      (stripPrefix "def " -> Just s) -> do
        (calc, (defs, h)) <- get
        let mdef = runExcept $ parseDef defs s
        case mdef of
          Right def -> let defs' = addDef defs def
                       in  put (calc, (defs', h))
          Left e -> lift . mapM_ outputStrLn $ showDefParseError e

      _ -> lift $ outputStrLn ("error: unknown metacommand `" ++ s ++ "`")

    runCalc :: String -> ICalc ()
    -- run the calculator on the input
    runCalc l = do
      (calc, (defs, h)) <- get
      let res = runExcept $ runStateT (runLine defs l) calc
      case res of
        Right (out, calc') -> do
          -- print output
          lift $ putout out
          -- update calc state and history
          put (calc', (defs, calc : h))
        Left e ->
          -- print error, do not change state
          lift $ putoute e
      where
        putout  = mapM_ ((outputStr " " >>) . outputStrLn)
        putoute = mapM_ outputStrLn . showE
          where
            showE = showError ePrintProg ePrintStack (T.pack l)


undo :: Int -> ICalc ()
-- pre: n >= 1
undo n = do
  (calc, (defs, h)) <- get
  put $ case drop (n - 1) h of
    -- found old state - update calc state and history
    (calc' : h') -> (calc',      (defs, h'))
    -- old state does not exist - fallback to initial state and clear history
    _            -> (emptyState, (defs, []))

