{-# LANGUAGE FlexibleContexts #-}

module Parser
  ( readExpr
  , readExprFile
  , SourceName)
  where

import           Lexer
import           LispVal

import           Control.Applicative
import           Control.Monad.Trans.Except
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type SourceName = String

stringP :: Parser LispVal
stringP = do
  char '"'
  x <- many $ escapedChars <|> noneOf ("\"\\" :: String)
  char '"'
  return $ String $ T.pack x

atomP :: Parser LispVal
atomP = do
  i <- identifier
  return $ case i of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom i

characterP :: Parser LispVal
characterP = do
  _ <- try $ string "#\\"
  value <- do { x <- anyChar; notFollowedBy alphaNumChar ; return [x] }
  return $ Character $ case value of
    "space"   -> ' '
    "newline" -> '\n'
    _         -> head value

negativeNumberP :: Parser LispVal
negativeNumberP = do
  char '-'
  i <- integer
  return $ Number (-1 * i)

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

manyLispVal :: Parser [LispVal]
manyLispVal = lispVal `sepBy` sc

listP :: Parser LispVal
listP = List <$> parens manyLispVal

quotedP :: Parser LispVal
quotedP = do
  char '\''
  x <- lispVal
  return $ List [Atom "quote", x]

lispVal :: Parser LispVal
lispVal = atomP
   <|> stringP
   <|> try floatP
   <|> try numberP
   <|> try boolP
   <|> try characterP
   <|> quotedP
   <|> listP

contents :: Parser a -> Parser a
contents p = do
  sc
  r <- p
  eof
  return r

parseUnpack :: Either (ParseError Char Void) LispVal -> Either String LispVal
parseUnpack err = case err of
  Left err  -> Left $ parseErrorPretty err
  Right ast -> Right ast

readExpr :: T.Text -> Either String LispVal
readExpr = parseUnpack . runParser (contents lispVal) "<stdin>"

readExprFile :: SourceName -> T.Text -> Either String LispVal
readExprFile source = parseUnpack . runParser (contents (List <$> manyLispVal)) source
