module Eval where

import Control.Monad.Trans.Except

import Syntax
import Exception

eval :: LispVal -> ThrowsException LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func

apply :: String -> [LispVal] -> ThrowsException LispVal
apply func args = case lookup func primitives of
  Nothing -> throwE $ NotFunction "Unrecognized primitive function args" func
  Just f  -> return $ f args

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("symbol?", unaryOp symbolp)
  , ("number?" , unaryOp numberp)
  , ("bool?", unaryOp boolp)
  , ("list?" , unaryOp listp)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)] in
    if null parsed
      then 0
      else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _)   = Bool True
boolp   _          = Bool False
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool True
listp   _          = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s) = String s
symbol2string _ = String ""
string2symbol (String s) = Atom s
string2symbol _ = Atom ""