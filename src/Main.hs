module Main where

import           LispVal
import           Parser

import           Control.Monad
import           Control.Monad.Trans
import qualified Data.Text                as T
import           System.Console.Haskeline
import           Text.Megaparsec.Error

process :: T.Text -> IO ()
process line =
  case readExpr line of
    Left err -> putStrLn err
    Right ast -> print ast

main :: IO ()
main = runInputT defaultSettings loop
    where
      loop = do
        minput <- getInputLine "wyas> "
        case minput of
          Nothing    -> outputStrLn "Goodbye."
          Just input -> liftIO (process $ T.pack input) >> loop
