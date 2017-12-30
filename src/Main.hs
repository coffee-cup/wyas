module Main where

import Control.Monad.Trans
import System.Console.Haskeline
import Text.Megaparsec.Error

import Parser
import Eval

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> putStr $ parseErrorPretty err
    Right ex -> print $ eval ex

main :: IO ()
main = runInputT defaultSettings loop
    where
      loop = do
        minput <- getInputLine "wyas> "
        case minput of
          Nothing -> outputStrLn "Goodbye."
          Just input -> liftIO (process input) >> loop
