module Main where

import Inline
import Interactive
import Macros (getSavedMacros)

import System.Environment (getArgs)

main :: IO ()
main = do args <- getArgs
          ms   <- getSavedMacros
          if null args
             then runInteractive ms
             else runInline ms args

