{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval where

import           LispVal
import           Parser
import           Prim

import           Data.Map             as Map
import           Data.Monoid
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO

import           Control.Exception
import           Control.Monad.Reader

basicEnv :: EnvCtx
basicEnv = Map.fromList $ primEnv
  <> [ ("read", Fun $ IFunc $ unop readFn)
     , ("parse", Fun $ IFunc $ unop parseFn)]

readFn :: LispVal -> Eval LispVal
readFn (String txt) = lineToEvalForm txt
readFn val = throw $ TypeMismatch "read expects string, instead got: " val

parseFn :: LispVal -> Eval LispVal
parseFn (String txt) = either (throw . PError . show) return $ readExpr txt
parseFn val = throw $ TypeMismatch "parse expects string, instead got: " val

evalFile :: FilePath -> T.Text -> IO () -- program file
evalFile filePath fileExpr =
  runASTinEnv basicEnv (fileToEvalForm filePath fileExpr) >>= print

fileToEvalForm :: FilePath -> T.Text -> Eval LispVal
fileToEvalForm filePath input =
  either (throw . PError . show) evalBody $ readExprFile filePath input

runParseTest :: T.Text -> T.Text
runParseTest input =
  either (T.pack . show) (T.pack . show) $ readExpr input

safeExec :: IO a -> IO (Either String a)
safeExec m = do
  result <- Control.Exception.try m
  case result of
    Left (eTop :: SomeException) ->
      case fromException eTop of
        Just (enclosed :: LispException) -> return $ Left (show enclosed)
        Nothing                          -> return $ Left (show eTop)
    Right val -> return $ Right val

runASTinEnv :: EnvCtx -> Eval b -> IO b
runASTinEnv code action =
  runReaderT (unEval action) code

textToEvalForm :: T.Text -> Eval LispVal
textToEvalForm input = either (throw . PError . show) evalBody $ readExpr input

evalText :: T.Text -> IO ()
evalText textExpr = do
  res <- runASTinEnv basicEnv $ textToEvalForm textExpr
  print res

lineToEvalForm :: T.Text -> Eval LispVal
lineToEvalForm input =
  either (throw . PError . show) eval $ readExpr input

endOfList :: LispVal -> LispVal -> LispVal
endOfList (List x) expr = List $ x ++ [expr]
endOfList n _ =          throw $ TypeMismatch "failure to get variables: " n

getVar :: LispVal -> Eval LispVal
getVar (Atom atom) = do
  env <- ask
  case Map.lookup atom env of
    Just x  -> return x
    Nothing -> throw $ UnboundVar atom
getVar n = throw $ TypeMismatch "failure to get variable: " n

ensureAtom :: LispVal -> Eval LispVal
ensureAtom n@(Atom txt) = return n
ensureAtom n            = throw $ TypeMismatch "expected an atomic value" n

extractVar :: LispVal -> T.Text
extractVar (Atom atom) = atom
extractVar n           = throw $ TypeMismatch "expected an atomic value" n

eval :: LispVal -> Eval LispVal
eval val@(String _)             = return val
eval val@(Number _)             = return val
eval val@(Float _)              = return val
eval val@(Bool _)               = return val
eval val@(Character _)          = return val
eval n@(Atom _)                 = getVar n
eval Nil                        = return Nil
eval (List [])                  = return Nil
eval (List [Atom "quote", val]) = return val
eval (List [Atom "write", rest]) =
  return . String . T.pack $ show rest
eval (List ((:) (Atom "write") rest)) =
  return . String . T.pack . show $ List rest

eval (List [Atom "if", pred, truExpr, flsExpr]) = do
  ifRes <- eval pred
  case ifRes of
    (Bool True)  -> eval truExpr
    (Bool False) -> eval flsExpr
    _            -> throw $ BadSpecialForm "if's first arg must eval into a boolean"
eval args@(List ( (:) (Atom "if") _)) = throw $ BadSpecialForm "(if <bool> <s-expr> <s-expr>)"

eval (List [Atom "begin", rest]) = evalBody rest
eval (List ((:) (Atom "begin") rest)) = evalBody $ List rest

eval (List [Atom "define", varExpr, expr]) = do
  env <- ask
  varAtom <- ensureAtom varExpr
  evalVal <- eval expr
  local (const $ Map.insert (extractVar varAtom) evalVal env) $ return varExpr

eval (List [Atom "let", List pairs, expr]) = do
  env <- ask
  atoms <- mapM ensureAtom $ getEven pairs
  vals <- mapM eval $ getOdd pairs
  local (const (Map.fromList (zipWith (\a b -> (extractVar a, b)) atoms vals) <> env))  $ evalBody expr
eval (List (Atom "let":_) ) = throw $ BadSpecialForm "let function expects list of parameters and S-Expression body\n(let <pairs> <s-expr>)"

eval (List [Atom "lambda", List params, expr]) = do
  envLocal <- ask
  return  $ Lambda (IFunc $ applyLambda expr params) envLocal
eval (List (Atom "lambda":_) ) = throw $ BadSpecialForm "lambda function expects list of parameters and S-Expression body\n(lambda <params> <s-expr>)"

eval all@(List ((:) x xs)) = do
  env <- ask
  funVar <- eval x
  case funVar of
    (Fun (IFunc internalFn)) -> mapM eval xs >>= internalFn
    (Lambda (IFunc definedFn) boundenv) -> local (const (boundenv <> env)) $ definedFn xs
    _ -> throw $ NotFunction funVar

eval x = throw $ Default x -- fall through

evalBody :: LispVal -> Eval LispVal
-- (define var expr rest)
evalBody (List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
  evalVal <- eval defExpr
  env <- ask
  local (const $ Map.insert var evalVal env) $ eval rest
-- (define var expr rest)
evalBody (List ((:) (List ((:) (Atom "define") [Atom var, defExpr])) rest)) = do
  evalVal <- eval defExpr
  env <- ask
  let envFn = const $ Map.insert var evalVal env
  local envFn $ evalBody $ List rest
evalBody x = eval x

getEven :: [t] -> [t]
getEven []     = []
getEven (x:xs) = x : getOdd xs

getOdd :: [t] -> [t]
getOdd []     = []
getOdd (x:xs) = getEven xs

applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda expr params args = do
  env <- ask
  argEval <- mapM eval args
  let env' = Map.fromList (Prelude.zipWith (\a b -> (extractVar a,b)) params argEval) <> env
  local (const env') $ eval expr
