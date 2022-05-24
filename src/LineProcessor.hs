{-# LANGUAGE ViewPatterns #-}

module LineProcessor (processLine) where

import Rpn (Instr, Instructions)
import Parser
import Macros
import Error
import Util (stripBrackets)

import Control.Monad
import Data.Bifunctor
import qualified Data.Map as M


processLine :: Macros -> [String] -> Result Instructions
-- process a line of instructions in preperations for being run
-- expand macros -> parse tokens -> set up jumptable
processLine ms l = do
  let l' = expandMacros ms l
  tokens <- mapM parseInstr l'
  let is = zip tokens l'
  return is

