module Parser where

import Control.Applicative
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr

import Syntax
import Lexer

stringP :: Parser LispVal
stringP = do
  char '"'
  x <- many $ escapedChars <|> noneOf ("\"\\" :: String)
  char '"'
  return $ String x

atomP :: Parser LispVal
atomP = do
  first <- letterChar <|> symbol
  rest <- many (letterChar <|> digitChar <|> symbol)
  let atom' = first:rest
  return $ case atom' of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom'

characterP :: Parser LispVal
characterP = do
  try $ string "#\\"
  value <- try (string "newline" <|> string "space")
         <|> do { x <- anyChar; notFollowedBy alphaNumChar ; return [x] }
  return $ Character $ case value of
    "space"   -> ' '
    "newline" -> '\n'
    _         -> head value

numberP :: Parser LispVal
numberP = do
  i <- try integer <|> try hexadecimal <|> try octal
  return $ Number i

floatP :: Parser LispVal
floatP = do
  f <- float
  return $ Float f

boolP :: Parser LispVal
boolP = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

expr :: Parser LispVal
expr = atomP
   <|> stringP
   <|> try floatP
   <|> try numberP
   <|> try boolP
   <|> try characterP

contents :: Parser a -> Parser a
contents p = do
  sc
  r <- p
  eof
  return r

readExpr :: String -> Either (ParseError Char Void) LispVal
readExpr = runParser (contents expr) "<stdin>"
