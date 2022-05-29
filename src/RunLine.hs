module RunLine where

import Rpn (rpn, Calc, CalcState, emptyState)
import Parser
import Macros
import Error
import Util ((.:))

import Control.Monad.State
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T


runLine :: Macros -> String -> Calc Error CalcState [String]
-- run a line in the calculator
runLine ms l = do
  -- expand macros
  let l' = T.pack $ expandMacros ms l

  -- parse line
  is <- lift . withExcept ParseE $ parseInstructions l'

  -- run calc with state
  mapStateT (withExcept CalcE) $ rpn is


runSingle :: Macros -> String -> Except Error ([String], CalcState)
-- run a single line with the empty state
runSingle = flip runStateT emptyState .: runLine

