module Main where

import Inline
import Interactive
import Macros (getSavedMacros)

import Options.Applicative


main = start =<< execParser (info (args <**> helper) desc)
  where
    desc = fullDesc
           <> header "rpnhs - A reverse Polish notation calculator in Haskell"
           <> progDesc "To run the calculator inline, provide instructions as cmdline args. \
                       \To run the calculator interactively, provide no cmdline args."
    args = Args
           <$> switch (long "auto-print" <> short 'p' <> help "Auto print if no output is produced from inline mode")
           <*> strOption (long "prompt" <> value "> " <> metavar "PROMPT" <> help "Set the interactive mode prompt")
           <*> optional (strOption (long "macro-file" <> short 'm' <> metavar "FILE" <> help "Load a given macro file on startup"))
           <*> many (strArgument mempty)

data Args = Args {
  autoPrint :: Bool,
  prompt    :: String,
  macroFile :: Maybe FilePath,
  inlineIn  :: [String]
}

start :: Args -> IO ()
start (Args autoPrint prompt macroFile inlineIn) = do
  ms <- getSavedMacros macroFile
  if null inlineIn
     then runInteractive ms prompt
     else runInline ms autoPrint inlineIn

