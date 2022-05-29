module RunLine where

import Rpn (rpn, Calc, CalcState, emptyState, Defs)
import Parser
import Error
import Util ((.:))

import Control.Monad.State
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T


runLine :: Defs -> String -> Calc Error CalcState [String]
-- run a line in the calculator
runLine defs l = do
  let l' = T.pack l

  -- parse line
  is <- lift . withExcept ParseE $ parseInstructions defs l'

  -- run calc with state
  mapStateT (withExcept CalcE) $ rpn is


runSingle :: Defs -> String -> Except Error ([String], CalcState)
-- run a single line with the empty state
runSingle = flip runStateT emptyState .: runLine

