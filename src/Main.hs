module Main where

import Control.Monad.Trans
import System.Console.Haskeline
import Text.Megaparsec.Error

process :: String -> IO ()
process line = do
  putStrLn line

main :: IO ()
main = runInputT defaultSettings loop
    where
      loop = do
        minput <- getInputLine "calc> "
        case minput of
          Nothing -> outputStrLn "Goodbye."
          Just input -> liftIO (process input) >> loop
