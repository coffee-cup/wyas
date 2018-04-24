{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Eval

import           Control.Monad.Trans
import qualified Data.Text                as T
import           System.Console.Haskeline

type Repl a = InputT IO a

main :: IO ()
main = runInputT defaultSettings repl

repl :: Repl ()
repl = do
  minput <- getInputLine "wyas> "
  case minput of
    Nothing -> outputStrLn "Goodbye."
    Just input -> liftIO (process input) >> repl

process :: String -> IO ()
process str = do
  res <- safeExec $ evalText $ T.pack str
  either putStrLn return res

processToAST :: String -> IO ()
processToAST  str = print $ runParseTest $ T.pack str
