module Main where

import           Data.Text.IO        as TIO
import           Eval
import           Options.Applicative
import           Repl
import           System.Directory

runScript :: FilePath -> IO ()
runScript fname = do
  exists <- doesFileExist fname
  if exists
    then TIO.readFile fname >>= evalFile fname
    else TIO.putStrLn "File does not exists."

data LineOpts = UseReplLineOpts | RunScriptLineOpts String

parseLineOpts :: Parser LineOpts
parseLineOpts = runScriptOpt <|> runReplOpt
  where
    runScriptOpt =
      RunScriptLineOpts <$> strOption (long "script"
                                       <> short 's'
                                       <> metavar "SCRIPT"
                                       <> help "File containing the script you want to run")
    runReplOpt =
      UseReplLineOpts <$ flag' () (long "repl"
                                   <> short 'r'
                                   <> help "Run as interavtive read/evaluate/print/loop")

schemeEntryPoint :: LineOpts -> IO ()
schemeEntryPoint UseReplLineOpts            = Repl.mainLoop --repl
schemeEntryPoint (RunScriptLineOpts script) = runScript script

main :: IO ()
main = execParser opts >>= schemeEntryPoint
  where
    opts = info (helper <*> parseLineOpts)
      ( fullDesc
     <> header "Executable binary for Write You A Scheme v2.0"
     <> progDesc "contains an entry point for both running scripts and repl" )
