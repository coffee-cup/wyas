{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module LispVal
  ( LispVal(..)
  , Eval(..)
  , IFunc(..)
  , EnvCtx
  , LispException(..)
  , showVal
  ) where

import qualified Data.Map             as Map
import qualified Data.Text            as T
import           Data.Typeable        (Typeable)

import           Control.Exception
import           Control.Monad.Reader

type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a }
  deriving (Monad, Functor, Applicative, MonadReader EnvCtx, MonadIO)

data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | Float Double
  | String T.Text
  | Bool Bool
  | Character Char
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  deriving (Typeable, Eq)

instance Show LispVal where
  show = T.unpack . showVal

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }
deriving (Typeable)

instance Eq IFunc where
  (==) _ _ = False

showVal :: LispVal -> T.Text
showVal val =
  case val of
    (Atom atom)     -> atom
    (String txt)    -> T.concat [ "\"" , txt, "\""]
    (Number num)    -> T.pack $ show num
    (Float num)     -> T.pack $ show num
    (Bool True)     -> "#t"
    (Bool False)    -> "#f"
    (Character c)   -> T.pack $ show c
    (List contents) -> T.concat ["(", unwordsList contents, ")"]
    (Fun _ )        -> "(internal function)"
    (Lambda _ _)    -> "(lambda function)"
    Nil             -> "'()"

unwordsList :: [LispVal] -> T.Text
unwordsList list = T.unwords $ showVal <$> list

data LispException
  = NumArgs Integer [LispVal]
  | LengthOfList T.Text Int
  | ExpectedList T.Text
  | TypeMismatch T.Text LispVal
  | BadSpecialForm T.Text
  | NotFunction LispVal
  | UnboundVar T.Text
  | Default LispVal
  | PError String
  | IOError T.Text
  deriving (Typeable)

instance Exception LispException

instance Show LispException where
  show = T.unpack . showError

showError :: LispException -> T.Text
showError err =
  case err of
    (IOError txt)            -> T.concat ["Error reading file: ", txt]
    (NumArgs int args)       -> T.concat ["Error Number Arguments, expected ", T.pack $ show int, " recieved args: ", unwordsList args]
    (LengthOfList txt int)   -> T.concat ["Error Length of List in ", txt, " length: ", T.pack $ show int]
    (ExpectedList txt)       -> T.concat ["Error Expected List in funciton ", txt]
    (TypeMismatch txt val)   -> T.concat ["Error Type Mismatch: ", txt, showVal val]
    (BadSpecialForm txt)     -> T.concat ["Error Bad Special Form: ", txt]
    (NotFunction val)        -> T.concat ["Error Not a Function: ", showVal val]
    (UnboundVar txt)         -> T.concat ["Error Unbound Variable: ", txt]
    (PError txt)             -> T.concat ["Parser Error, expression cannot evaluate: ", T.pack txt]
    (Default val)            -> T.concat ["Error, Danger Will Robinson! Evaluation could not proceed!  ", showVal val]
