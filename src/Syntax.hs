module Syntax where

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Double
  | String String
  | Bool Bool
  | Character Char
  deriving (Eq, Ord, Show)