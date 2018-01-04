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
  deriving (Eq, Ord)

instance Show LispVal where
  show (String contents) = "\"" ++ contents ++ "\""
  show (Atom name) = name
  show (Number contents) = show contents
  show (Float contents) = show contents
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Character c) = show c
  show (List contents) = "(" ++ unwordsList contents ++ ")"
  show (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ show t ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show
