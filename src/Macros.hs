module Macros where

import Control.Monad
import Control.Error
import Data.List
import System.IO
import System.Environment


type Macros = [Macro]
-- pair of macro and its expansion
-- e.g. ("triple", ["3", "*"])
type Macro  = (String, [String])

noMacros = []


expandMacros :: Macros -> [String] -> [String]
-- expand any occurrences of macros in the strings
-- expansion is performed only once
expandMacros ms = concatMap expand
  where
    expand s = case lookup s ms of
                 Just ss -> ss
                 _       -> [s]


parseMacros :: [String] -> Maybe Macros
parseMacros = mapM parseMacro . filter macroLine
  where
    macroLine ('#' : _) = False
    macroLine ""        = False
    macroLine _         = True

parseMacro :: String -> Maybe Macro
parseMacro = parse . filter (not . null) . words
  where
    parse (m : ss) | not (null ss) = Just (m, ss)
    parse _                        = Nothing


getSavedMacros :: IO Macros
-- read saved macros from a config file
-- the following paths are tried in order
--   $RPNHS_MACRO_FILE
--   ~/.rpnhs_macros
--   ~/.config/rpnhs/macros
--   $XDG_CONFIG_HOME/rpnhs/macros
-- if no config file can be found, no macros are loaded
-- if an invalid config file is found, no macros are loaded and no other paths are tried
getSavedMacros = handleConf <$> getConfig
  where
    handleConf (Just conf) = fromMaybe noMacros . parseMacros . filter (not . null) $ lines conf
    handleConf _           = noMacros

    getConfig :: IO (Maybe String)
    getConfig = headMay . catMaybes <$> confs

    confs :: IO [Maybe String]
    confs = mapM tryFile [tryIO $ getEnv "RPNHS_MACRO_FILE",
                          (<> "/.rpnhs_macros")        <$> home,
                          (<> "/.config/rpnhs/macros") <$> home,
                          (<> "/rpnhs/macros")         <$> tryIO (getEnv "XDG_CONFIG_HOME")]
      where
        tryFile :: ExceptT IOError IO FilePath -> IO (Maybe String)
        -- try to read the contents of a file at a path
        tryFile ep = fmap hush . runExceptT $ (tryIO . readFile =<< ep)
        home = tryIO $ getEnv "HOME"
