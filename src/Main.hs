module Main where

import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline
import Text.Megaparsec.Error

import Syntax
import Parser
import Eval
import Exception

getAst :: String -> IO (ThrowsException LispVal)
getAst line = return $ parseExpr line

process :: String -> IO ()
process line =
  putStrLn $ trapException $ parseExpr line >>= eval

main :: IO ()
main = runInputT defaultSettings loop
    where
      loop = do
        minput <- getInputLine "wyas> "
        case minput of
          Nothing -> outputStrLn "Goodbye."
          Just input -> liftIO (process input) >> loop
