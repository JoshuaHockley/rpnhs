{-# LANGUAGE TupleSections #-}

module Rpn where

import Value
import Operator
import Bases
import Error
import Util (pullElem, pushElem)

import Control.Monad
import Data.Bifunctor (first, second)
import Data.Maybe (maybeToList)


-- the state of the calculator
type State = (Stack, Vars)

emptyState = ([], [])

-- a valid token
data Token = TokenPure TokenPure
           | CmdPrintT CommandPrint
           | Jump Bool String  -- jump to a label
                               -- if flag is set, only jump when the top of the stack is non-zero
           | RetT
           | ErrT

-- a token that has a pure effect on the state
data TokenPure = ValT Value
               | OpT  Operator
               | CmdT Command

data Operator = Op1 Operator1
              | Op2 Operator2
              | OpF Operator2  -- folding operator

-- a command that modifies the state
data Command = Pop Int        -- remove the top n values from the stack
             | Clear          -- clear all values from the stack
             | Dup Int        -- repush the value at the top of the stack n times
             | Pull Int       -- pull the nth value (n >= 1) to the top of the stack
             | Push Int       -- push the head of the stack to the nth position
             | Store String   -- pop the top of the stack and store it in a variable
             | Load String    -- load a variable onto the stack
             | Depth          -- push the depth of the stack

-- a command that produces an string to print
data CommandPrint = Print (Maybe (Int, Bool))  -- print the value at the top of the stack
                                               -- optional base desc (base, use radix complement)
                  | Stack                      -- print the entire stack
                  | View String                -- print the value of a variable
                  | ViewAll                    -- print all variables with their values

-- mapping of identifiers to Values
type Vars = [(String, Value)]

-- instructions for the calculator to execute
-- the String is the source of the Token, used to provide context to errors
type Instructions = [(Token, String)]

-- mapping of labels to their target programs
type JumpTable = [(String, Instructions)]


rpn :: JumpTable -> State -> Instructions -> Result (State, [String])
-- run the calulator on a list of instructions
-- returns the final state and a list of output to print
rpn jtable = rpn'
  where
    rpn' :: State -> Instructions -> Result (State, [String])
    rpn' st@(s, _) ts'@((t, str) : ts)
      = case t of
          -- execute a pure token on the state
          TokenPure t -> do
            st' <- withContext' $ processTokenPure t st
            rpn' st' ts

          -- record output from a print command
          CmdPrintT c -> do
            l <- withContext' $ runCmdPrint c st
            (st', out) <- rpn' st ts
            return (st', l ++ out)

          -- conditional jump
          Jump True l -> do
            cond <- checkCond st
            let st' = first (drop 1) st  -- pop cond value
            if cond then jump st' l
                    else rpn' st' ts

          -- unconditional jump
          Jump _    l -> jump st l

          -- RET
          RetT -> Ok (st, [])

          -- ERR
          ErrT -> withContext' $ mkErr UserErrorE

      where
        withContext' :: Result a -> Result a
        -- inject the current context into an error
        withContext' = withContext (map snd ts', s)

        jump :: State -> String -> Result (State, [String])
        -- perform a jump to a label
        jump st l = rpn' st =<< withContext' (toResult (UndefinedLabelE l) (lookup l jtable))

        checkCond :: State -> Result Bool
        checkCond (v : _, _) = Ok . not $ isZero v
        checkCond _          = withContext' $ mkErr EmptyStackE

    rpn' st _ = Ok (st, [])



processTokenPure :: TokenPure -> State -> Result State
-- process a pure token with the state
processTokenPure (ValT v) (s, vars) = Ok (v : s, vars)
processTokenPure (OpT op) (s, vars) = (, vars) <$> toResult OperatorFailureE (processOp op s)
processTokenPure (CmdT c) st        = runCmd c st


processOp :: Operator -> Stack -> Maybe Stack
-- process an operator on the stack
-- returns Nothing iff there are not enough operands, or the values are of invalid types
processOp (Op1 op) (v : s)      = (: s)  <$> op v
processOp (Op2 op) (v : v' : s) = (: s)  <$> op v' v
processOp (OpF op) (v : v' : s) = return <$> foldM op v (v' : s)
processOp _        _            = Nothing


runCmd :: Command -> State -> Result State
-- run a command
runCmd (Pop n)      (s, vars)     = Ok (drop n s, vars)
runCmd Clear        (s, vars)     = Ok ([], vars)
runCmd (Dup n)      (v : s, vars) = Ok (replicate (n + 1) v ++ s, vars)
runCmd (Dup _)      _             = mkErr EmptyStackE
runCmd (Pull n)     (s, vars)     = (, vars) <$> toResult PullE (pullElem (n - 1) s)
runCmd (Push n)     (s, vars)     = (, vars) <$> toResult PushE (pushElem n       s)
runCmd Depth        (s, vars)     = Ok (I d : s, vars) where d = toInteger (length s)
runCmd (Store iden) (v : s, vars) = Ok (s, setVar iden v vars)
runCmd (Store _)    _             = mkErr EmptyStackE
runCmd (Load iden)  (s, vars)     = (, vars) . (: s) <$> getVar iden vars


runCmdPrint :: CommandPrint -> State -> Result [String]
-- run an print command from the state
runCmdPrint (Print desc) (v : _, _)
  = case desc of
      Just (base, compl) -> return . showB compl base <$> toResult PrintBaseNonIntegerE (asI v)
      _                  -> Ok . return $ show v
runCmdPrint (Print _)   _         = mkErr EmptyStackE
runCmdPrint Stack       (s, _)    = Ok . maybeToList $ showStack s
runCmdPrint (View iden) (_, vars) = return . show <$> getVar iden vars
runCmdPrint ViewAll     (_, vars) = Ok $ map showVar vars

-- vars
getVar :: String -> Vars -> Result Value
getVar iden = toResult (UndefinedVarE iden) . lookup iden

setVar :: String -> Value -> Vars -> Vars
-- set a variable, overwriting if the identifier is already in use
setVar iden v = ((iden, v) :) . filter ((/= iden) . fst)

showVar :: (String, Value) -> String
showVar (iden, v) = iden ++ " = " ++ show v

