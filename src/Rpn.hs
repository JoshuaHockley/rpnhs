{-# LANGUAGE TupleSections #-}

module Rpn where

import Value
import Operator
import Bases
import Error
import Util (pullElem, pushElem)

import Control.Monad
import Data.Bifunctor (first, second)
import Data.Maybe (maybeToList, listToMaybe, catMaybes)
import qualified Data.Map as M


-- the state of the calculator
type State = (Stack, Vars)

emptyState = ([], M.empty) :: State


-- an instruction
data Instr = InstrPure InstrPure
           | CommandPrint CommandPrint
           | Err

-- an instruction that has a pure effect on the state
data InstrPure = Value Value
               | Command Command
               | Operator Operator

-- a command that modifies the state
data Command = Pop Int        -- remove the top n values from the stack
             | Clear          -- clear all values from the stack
             | Dup Int        -- repush the value at the top of the stack n times
             | Pull Int       -- pull the nth value (n >= 1) to the top of the stack
             | Push Int       -- push the head of the stack to the nth position
             | Store String   -- pop the top of the stack and store it in a variable
             | Load String    -- load a variable onto the stack
             | Depth          -- push the depth of the stack

data Operator = Op1 Operator1  -- unary operator
              | Op2 Operator2  -- binary operator
              | OpF Operator2  -- folding operator

-- a command that produces an string to print
data CommandPrint = Print (Maybe (Int, Bool))  -- print the value at the top of the stack
                                               -- optional base desc (base, use radix complement)
                  | Stack                      -- print the entire stack
                  | View String                -- print the value of a variable
                  | ViewAll                    -- print all variables with their values

-- mapping of identifiers to Values
type Vars = M.Map String Value

-- instructions for the calculator to execute
-- the String is the source of the Token, used to provide context to errors
type Instructions = [(Instr, String)]


rpn :: State -> Instructions -> Result (State, [String])
-- run the calulator on a list of instructions
-- returns the final state and a list of output to print
rpn = rpn'
  where
    rpn' :: State -> Instructions -> Result (State, [String])
    rpn' st@(s, _) is'@((i, _) : is)
      = case i of
          -- execute a pure instruction on the state
          InstrPure i -> do
            st' <- withContext' $ processInstrPure i st
            rpn' st' is

          -- record output from a print command
          CommandPrint c -> do
            l <- withContext' $ runCmdPrint c st
            (st', out) <- rpn' st is
            return (st', l ++ out)

          -- ERR
          Err -> withContext' $ mkErr UserErrorE

      where
        withContext' :: Result a -> Result a
        -- inject the current context into an error
        withContext' = withContext (map snd is', s)

        checkCond :: State -> Result Bool
        checkCond (v : _, _) = return . not $ isZero v
        checkCond _          = withContext' $ mkErr EmptyStackE

    rpn' st _ = return (st, [])


processInstrPure :: InstrPure -> State -> Result State
-- process a pure instruction with the state
processInstrPure (Value v)     (s, vars) = return (v : s, vars)
processInstrPure (Command c)   st        = runCmd c st
processInstrPure (Operator op) (s, vars) = (, vars) <$> processOp op s


runCmd :: Command -> State -> Result State
-- run a command
runCmd (Pop n)      (s, vars)     = return (drop n s, vars)
runCmd Clear        (s, vars)     = return ([], vars)
runCmd (Dup n)      (v : s, vars) = return (replicate (n + 1) v ++ s, vars)
runCmd (Dup _)      _             = mkErr EmptyStackE
runCmd (Pull n)     (s, vars)     = (, vars) <$> toResult PullE (pullElem (n - 1) s)
runCmd (Push n)     (s, vars)     = (, vars) <$> toResult PushE (pushElem n       s)
runCmd Depth        (s, vars)     = return (I d : s, vars) where d = toInteger (length s)
runCmd (Store iden) (v : s, vars) = return (s, setVar iden v vars)
runCmd (Store _)    _             = mkErr EmptyStackE
runCmd (Load iden)  (s, vars)     = (, vars) . (: s) <$> getVar iden vars


processOp :: Operator -> Stack -> Result Stack
-- process an operator on the stack
processOp (Op1 op) (v : s)      = (: s)  <$> toResult OperatorFailureE (op v)
processOp (Op2 op) (v : v' : s) = (: s)  <$> toResult OperatorFailureE (op v' v)
processOp (OpF op) (v : v' : s) = return <$> toResult OperatorFailureE (foldM op v (v' : s))
processOp _        _            = mkErr NotEnoughOperandsE


runCmdPrint :: CommandPrint -> State -> Result [String]
-- run an print command from the state
runCmdPrint (Print desc) (v : _, _)
  = case desc of
      Just (base, compl) -> return . showB compl base <$> toResult PrintBaseNonIntegerE (asI v)
      _                  -> return . return $ show v
runCmdPrint (Print _)   _         = mkErr EmptyStackE
runCmdPrint Stack       (s, _)    = return . maybeToList $ showStack s
runCmdPrint (View iden) (_, vars) = return . show <$> getVar iden vars
runCmdPrint ViewAll     (_, vars) = return $ showVars vars


-- vars
getVar :: String -> Vars -> Result Value
getVar iden = toResult (UndefinedVarE iden) . M.lookup iden

setVar :: String -> Value -> Vars -> Vars
-- set a variable, overwriting if the identifier is already in use
setVar = M.insert

showVars :: Vars -> [String]
showVars = map showVar . M.toAscList
  where
    showVar (iden, v) = iden ++ " = " ++ show v

