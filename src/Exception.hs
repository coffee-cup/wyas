module Exception
  ( LispException(..)
  , ThrowsException
  , trapException
  ) where

import Control.Exception
import Control.Monad.Trans.Except

import Data.Void
import Text.Megaparsec

import Syntax

data LispException
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | ParserE (ParseError Char Void)
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

instance Show LispException where
  show (NumArgs expected found) =
    "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
  show (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected
    ++ ", found " ++ show found
  show (ParserE parseErr) = parseErrorPretty parseErr
  show (BadSpecialForm message form) =
    message ++ ": " ++ show form
  show (NotFunction message func) =
    message ++ ": " ++ show func
  show (UnboundVar message varname) =
    message ++ ": " ++ varname
  show (Default message) = message

instance Exception LispException

type ThrowsException = Except LispException

trapException :: ThrowsException LispVal -> String
trapException action = either show show $ runExcept action
