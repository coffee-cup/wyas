module Parser where

import Control.Monad.Trans.Except
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.Char

import Syntax
import Exception
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
  _ <- try $ string "#\\"
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

listP :: Parser LispVal
listP = fmap List $ exprP `sepBy` sc

dottedListP :: Parser LispVal
dottedListP = do
  h <- exprP `endBy` sc
  t <- char '.' >> sc >> exprP
  return $ DottedList h t

quotedP :: Parser LispVal
quotedP = do
  char '\''
  x <- exprP
  return $ List [Atom "quote", x]

quasiQuoted :: Parser LispVal
quasiQuoted = do
  char '`'
  x <- exprP
  return $ List [Atom "quasiquote", x]

unQuoteP :: Parser LispVal
unQuoteP = do
  char ','
  x <- exprP
  return $ List [Atom "unquote", x]

exprP :: Parser LispVal
exprP = atomP
   <|> stringP
   <|> try floatP
   <|> try numberP
   <|> try boolP
   <|> try characterP
   <|> quotedP
   <|> quasiQuoted
   <|> unQuoteP
   <|> do
      char '('
      x <- try listP <|> dottedListP
      char ')'
      return x

contents :: Parser a -> Parser a
contents p = do
  sc
  r <- p
  eof
  return r

parseExpr :: String -> ThrowsException LispVal
parseExpr s = case runParser (contents exprP) "<stdin>" s of
  Left err -> throwE $ ParserE err
  Right val -> return val

