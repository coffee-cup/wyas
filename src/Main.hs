module Main where

import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline
import Text.Megaparsec.Error

import Syntax
import Parser
import Eval
import Exception

process :: String -> IO ()
process line = do
  ast <-  liftM $ parseExpr line
  putStrLn ast

main :: IO ()
main = runInputT defaultSettings loop
    where
      loop = do
        minput <- getInputLine "wyas> "
        case minput of
          Nothing -> outputStrLn "Goodbye."
          Just input -> liftIO (process input) >> loop
