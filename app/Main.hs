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
           <$> strOption (long "prompt" <> short 'P' <> value "> " <> metavar "PROMPT" <> help "Set the interactive mode prompt")
           <*> switch (long "auto-print" <> short 'p' <> help "Auto print if no output is produced from inline mode")
           <*> switch (long "eprint-prog" <> short 'i' <> help "Print the program marking the current instruction when an error occurs")
           <*> switch (long "eprint-stack" <> short 's' <> help "Print the stack when an error occurs")
           <*> optional (strOption (long "macro-file" <> short 'm' <> metavar "FILE" <> help "Load a given macro file on startup"))
           <*> many (strArgument mempty)

data Args = Args {
  prompt      :: String,
  autoPrint   :: Bool,
  ePrintInstr :: Bool,
  ePrintStack :: Bool,
  macroFile   :: Maybe FilePath,
  inlineIn    :: [String]
}

start :: Args -> IO ()
start (Args prompt autoPrint ePrintInstr ePrintStack macroFile inlineIn) = do
  ms <- getSavedMacros macroFile
  if null inlineIn
     then runInteractive ms prompt    ePrintInstr ePrintStack
     else runInline      ms autoPrint ePrintInstr ePrintStack (unwords inlineIn)

