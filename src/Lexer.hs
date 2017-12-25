{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Lexer where

import Control.Applicative
import Data.Void
import Data.Functor.Identity
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment ";"
    blockCmnt = empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Parser Char
symbol = oneOf ("!$%&|*+-/:<=>?@^_~" :: String)

escapedChars :: Parser Char
escapedChars = do
  char '\\'                -- a backslash
  x <- oneOf ("\\\"nrt" :: String) -- either backslash or doublequote
  return $ case x of
    '\\'  -> x
    '"'   -> x
    'n'   -> '\n'
    'r'   -> '\r'
    't'   -> '\t'
    _     -> x

integer :: Parser Integer
integer = lexeme $ (char '#' >> char 'd' >> L.decimal) <|> L.decimal

float :: Parser Double
float = lexeme L.float

octal :: Parser Integer
octal = lexeme $ char '#' >> char 'o' >> L.octal

hexadecimal :: Parser Integer
hexadecimal = lexeme $ char '#' >> char 'x' >> L.hexadecimal
