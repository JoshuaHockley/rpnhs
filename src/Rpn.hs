{-# LANGUAGE TupleSections #-}

module Rpn where

import Value
import Operator
import Bases
import Error
import Util ((.:), pullElem, pushElem)

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe (maybeToList, listToMaybe, catMaybes)
import Data.Tuple (swap)
import Data.Bifunctor (first, second)
import qualified Data.Map as M


-- instructions for the calculator to execute
type Instructions = [Instruction]

-- an instruction and it's position in the source
type Instruction = (Instr, Int)

-- an instruction
data Instr = Value Value
           | Operator Operator
           | Command Command
           | CommandPrint CommandPrint

-- a command that modifies the state
data Command = Pop Int        -- remove the top n values from the stack
             | Clear          -- clear all values from the stack
             | Dup Int        -- repush the value at the top of the stack n times
             | Pull Int       -- pull the nth value (n >= 1) to the top of the stack
             | Push Int       -- push the head of the stack to the nth position
             | Store String   -- pop the top of the stack and store it in a variable
             | Load String    -- load a variable onto the stack
             | Depth          -- push the depth of the stack
             deriving Show

data Operator = Op1 Operator1  -- unary operator
              | Op2 Operator2  -- binary operator

-- a command that produces output
data CommandPrint = Print (Maybe (Int, Bool))  -- print the value at the top of the stack
                                               --  optional base desc (base, use radix complement)
                  | Stack                      -- print the entire stack
                  | View (Maybe String)        -- print the value of a variable,
                                               --  or all variables and their values
                  deriving Show


-- monad stack for computation in the calculator
type Calc e s = StateT s (Except e)

type Calc' s  = Calc CalcError s

-- the state of the calculator
type CalcState = (Stack, Vars)

emptyState = ([], M.empty) :: CalcState

-- mapping of identifiers to Values
type Vars = M.Map String Value


rpn :: Instructions -> Calc CtxError CalcState [String]
-- run the calulator on a list of instructions
rpn = fmap concat . mapM runInstr


runInstr :: Instruction -> Calc CtxError CalcState [String]
runInstr (i, pos) = do
  -- prepare error context
  s <- onStack get
  let ctx = mapStateT . withExcept $ withContext (pos, s)
         :: Calc CalcError s a -> Calc CtxError s a

  -- run instruction and inject context into error
  ctx $ case i of
    Value v        -> noOutput . onStack $ push v
    Operator op    -> noOutput . onStack $ runOp op
    Command c      -> noOutput           $ runCmd c
    CommandPrint c -> runCmdPrint c
  where
    noOutput = fmap (const [])


runOp :: Operator -> Calc' Stack ()
runOp (Op1 op) = do
  v  <- pop
  v' <- unwrap OperatorFailureE $ op v
  push v'
runOp (Op2 op) = do
  (v', v) <- (,) <$> pop <*> pop
  v''     <- unwrap OperatorFailureE $ op v v'
  push v''


runCmd :: Command -> Calc' CalcState ()
runCmd (Pop n)
  = void . replicateM_ n $ onStack pop
runCmd Clear
  = onStack $ modify (const [])
runCmd (Dup n) = onStack $ do
  v <- peek
  modify (replicate n v ++)
runCmd (Pull n) = onStack $ do
  s  <- get
  s' <- unwrap PullE $ pullElem (n - 1) s
  put s'
runCmd (Push n) = onStack $ do
  s  <- get
  s' <- unwrap PushE $ pushElem n s
  put s'
runCmd Depth
  = onStack (push . I . toInteger . length =<< get)
runCmd (Store iden) = do
  v <- onStack pop
  onVars $ setVar iden v
runCmd (Load iden) = do
  v <- onVars (getVar iden)
  onStack $ push v


runCmdPrint :: CommandPrint -> Calc' CalcState [String]
runCmdPrint (Print desc) = do
  v <- onStack peek
  case desc of
    Just (base, compl) -> return . showB compl base <$> unwrap PrintBaseNonIntegerE (asI v)
    _                  -> return [show v]
runCmdPrint Stack
  = maybeToList . showStack <$> onStack get
runCmdPrint (View mIden)
  = case mIden of
      Just iden -> return . show <$> onVars (getVar iden)
      _         -> onVars showVars


-- state lifting

onStack :: Calc e Stack a -> Calc e CalcState a
onStack action = do
  (s, vars) <- get
  (x, s') <- lift $ runStateT action s
  put (s', vars)
  return x

onVars :: Calc e Vars a -> Calc e CalcState a
onVars action = do
  (s, vars) <- get
  (x, vars') <- lift $ runStateT action vars
  put (s, vars')
  return x


-- stack

push :: Value -> Calc' Stack ()
push v = modify (v :)

pop :: Calc' Stack Value
pop = do
  s <- get
  case s of
    (v : s') -> put s' >> return v
    _        -> throwError EmptyStackE

peek :: Calc' Stack Value
peek = do
  s <- get
  case s of
    (v : _) -> return v
    _       -> throwError EmptyStackE


-- vars

getVar :: String -> Calc' Vars Value
getVar iden = unwrap (UndefinedVarE iden) . M.lookup iden =<< get

setVar :: String -> Value -> Calc' Vars ()
-- set a variable, overwriting if the identifier is already in use
setVar = modify .: M.insert

showVars :: Calc' Vars [String]
showVars = map showVar . M.toAscList <$> get
  where
    showVar (iden, v) = iden ++ " = " ++ show v

