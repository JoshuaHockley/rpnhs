{-# LANGUAGE ViewPatterns #-}

module LineProcessor (processLine) where

import Rpn (Token, JumpTable)
import Parser
import Macros
import Error
import Util (stripBrackets)

import Control.Monad
import Data.Bifunctor


processLine :: Macros -> [String] -> Result ([Token], JumpTable)
-- process a line of instructions in preperations for being run
-- expand macros -> parse tokens -> set up jumptable
processLine ms l = do
  let l' = expandMacros ms l
  (ss, labels) <- extractLabels l'
  tokens <- mapM parseToken ss
  let jt = buildJumpTable tokens labels
  return (tokens, jt)


extractLabels :: [String] -> Result ([String], [(String, Int)])
extractLabels = fmap (\(_, ss, ls) -> (reverse ss, ls)) . foldM extract (0, [], [])
  where
    extract :: (Int, [String], [(String, Int)]) -> String -> Result (Int, [String], [(String, Int)])
    -- (index, tokens, (label, index))
    extract (i, ss, ls) (stripBrackets -> Just s) = do
      assert (s `notElem` map fst ls) (DuplicateLabelE s)
      return (i, ss, (s, i) : ls)
    extract (i, ss, ls) s = return (i + 1, s : ss, ls)

buildJumpTable :: [Token] -> [(String, Int)] -> JumpTable
buildJumpTable ts = map (second (`drop` ts))
