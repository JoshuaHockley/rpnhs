module Subroutine where

import Rpn (Instructions, Defs)
import Parser
import Error

import Control.Monad
import Control.Monad.Except
import Control.Error
import qualified Data.Map as M
import Data.List
import Data.List.Extra (word1, trimStart)
import qualified Data.Text as T
import System.IO
import System.Environment

import Util ((.:))

noDefs = M.empty :: Defs

-- pair of subroutine name and its meaning
-- e.g. ("triple", [3, Mul])
type Def  = (String, Instructions)

addDef :: Defs -> Def -> Defs
addDef defs (name, body) = M.insert name body defs


parseDef :: Defs -> String -> Except DefParseError Def
-- pre: input is non empty
parseDef defs s = do
  assert (EmptyBodyE s') $ not (null body)
  assert (InvalidSubroutineNameE name) $ validName name
  is <- withExcept ParseError $ parseInstructions defs (T.pack body)
  return (name, is)
  where
    s' = trimStart s
    (name, body) = word1 s'
    validName :: String -> Bool
    validName = all (\c -> c /= '(' && c /= ')')


parseDefs :: [String] -> Except DefParseError Defs
parseDefs = foldM (\defs -> fmap (addDef defs) . parseDef defs) noDefs
            . filter nonEmptyLine
  where
    parseLine :: String -> Except DefParseError Def
    parseLine s = undefined

    nonEmptyLine ('#' : _)          = False
    nonEmptyLine s | all (== ' ') s = False
    nonEmptyLine _                  = True


getSavedDefs :: Maybe FilePath -> IO Defs
-- read saved macros from a config file
-- the following paths are tried in order
--   filepath provided in Maybe
--   $RPNHS_DEFS
--   ~/.rpnhs_defs
--   ~/.config/rpnhs/defs
--   $XDG_CONFIG_HOME/rpnhs/defs
-- if no config file can be found, no macros are loaded
-- if an invalid config file is found, no macros are loaded and no other paths are tried
getSavedDefs fp = handleConf =<< getConfig fp
  where
    handleConf (Just conf)
      = case runExcept (parseDefs (lines conf)) of
          Right defs -> return defs
          Left  e    -> mapM_ putStrLn (showDefParseError e) >> return noDefs
    handleConf _ = return noDefs

    getConfig :: Maybe FilePath -> IO (Maybe String)
    getConfig (Just fp) = tryFile . tryIO $ readFile fp
    getConfig _         = headMay . catMaybes <$> confs

    confs :: IO [Maybe String]
    confs = mapM tryFile [tryIO $ getEnv "RPNHS_DEFS",
                          (<> "/.rpnhs_defs")        <$> home,
                          (<> "/.config/rpnhs/defs") <$> home,
                          (<> "/rpnhs/defs")         <$> tryIO (getEnv "XDG_CONFIG_HOME")]

    tryFile :: ExceptT IOError IO FilePath -> IO (Maybe String)
    -- try to read the contents of a file at a path
    tryFile ep = fmap hush . runExceptT $ (tryIO . readFile =<< ep)
    home = tryIO $ getEnv "HOME"
