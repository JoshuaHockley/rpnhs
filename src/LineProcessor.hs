{-# LANGUAGE ViewPatterns #-}

module LineProcessor (processLine) where

import Rpn (Instr, Instructions, JumpTable)
import Parser
import Macros
import Error
import Util (stripBrackets)

import Control.Monad
import Data.Bifunctor
import qualified Data.Map as M


processLine :: Macros -> [String] -> Result (Instructions, JumpTable)
-- process a line of instructions in preperations for being run
-- expand macros -> parse tokens -> set up jumptable
processLine ms l = do
  let l' = expandMacros ms l
  (ss, labels) <- extractLabels l'
  tokens <- mapM parseInstr ss
  let is = zip tokens ss
  let jt = buildJumpTable is labels
  return (is, jt)


extractLabels :: [String] -> Result ([String], [(String, Int)])
extractLabels = fmap (\(_, ss, ls) -> (reverse ss, ls)) . foldM extract (0, [], [])
  where
    extract :: (Int, [String], [(String, Int)]) -> String -> Result (Int, [String], [(String, Int)])
    -- (index, tokens, (label, index))
    extract (i, ss, ls) (stripBrackets -> Just s) = do
      assert (s `notElem` map fst ls) (DuplicateLabelE s)
      return (i, ss, (s, i) : ls)
    extract (i, ss, ls) s = return (i + 1, s : ss, ls)

buildJumpTable :: Instructions -> [(String, Int)] -> JumpTable
buildJumpTable is = M.fromList . map (second (`drop` is))
