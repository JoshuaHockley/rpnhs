{-# LANGUAGE ViewPatterns #-}

module LineProcessor (processLine) where

import Rpn (Instr, Instructions)
import Parser
import Macros
import Error

import Control.Monad
import Data.Bifunctor
import qualified Data.Text as T
import qualified Data.Map as M


processLine :: Macros -> String -> ParseResult Instructions
-- process a line of instructions in preperations for being run
-- expand macros -> parse instructions
processLine ms = parseInstructions . T.pack . expandMacros ms

