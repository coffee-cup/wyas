{-# LANGUAGE ExistentialQuantification #-}

module Eval where

import           Control.Monad
import           Control.Monad.Trans.Except

import           Exception
import           Syntax

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsException a)

eval :: LispVal -> ThrowsException LispVal
eval val@(String _)             = return val
eval val@(Atom _)               = return val
eval val@(Number _)             = return val
eval val@(Float _)              = return val
eval val@(Bool _)               = return val
eval val@(Character _)          = return val
eval (List [Atom "quote", val]) = return val
eval (List ((Atom "if") : args)) = ifE args
eval (List ((Atom "cond") : args)) = cond args
eval (List (Atom "case" : args)) = caseE args
eval (List (Atom func : args))  = mapM eval args >>= apply func


apply :: String -> [LispVal] -> ThrowsException LispVal
apply func args =
  case lookup func primitives of
    Nothing -> throwE $ NotFunction "Unrecognized primitive function args" func
    Just f -> f args

primitives :: [(String, [LispVal] -> ThrowsException LispVal)]
primitives =
  [ ("+", binaryOp (+))
  , ("-", binaryOp (-))
  , ("*", binaryOp (*))
  , ("/", binaryOp div)
  , ("mod", binaryOp mod)
  , ("quotient", binaryOp quot)
  , ("remainder", binaryOp rem)
  , ("symbol?", unaryOp symbolp)
  , ("number?", unaryOp numberp)
  , ("bool?", unaryOp boolp)
  , ("list?", unaryOp listp)
  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eq?", eqv)
  , ("eqv?", eqv)
  , ("equal?", equal)
  , ("string-length", stringLen)
  , ("string-ref", stringRef)
  ]

boolBinop :: (LispVal -> ThrowsException a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsException LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwE $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

binaryOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsException LispVal
binaryOp op []            = throwE $ NumArgs 2 []
binaryOp op singleVal@[_] = throwE $ NumArgs 2 singleVal
binaryOp op params        = (Number . foldl1 op) <$> mapM unpackNum params

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsException LispVal
unaryOp f [v] = return $ f v
unaryOp _ l   = throwE $ NumArgs 1 l

unpackNum :: LispVal -> ThrowsException Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)]
  in if null parsed
       then throwE $ TypeMismatch "number" $ String n
       else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwE $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsException String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwE $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsException Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwE $ TypeMismatch "boolean" notBool

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsException Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
  `catchE` (const $ return False)

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _) = Bool True
symbolp _        = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _) = Bool True
boolp   _        = Bool False
listp   (List _)         = Bool True
listp   (DottedList _ _) = Bool True
listp   _                = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s) = String s
symbol2string _        = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""

stringLen :: [LispVal] -> ThrowsException LispVal
stringLen [(String s)] = return $ Number $ fromIntegral $ length s
stringLen [notString] = throwE $ TypeMismatch "string" notString
stringLen badArgList = throwE $ NumArgs 1 badArgList

stringRef :: [LispVal] -> ThrowsException LispVal
stringRef [(String s), (Number k)]
  | length s < k' + 1 = throwE $ Default "Out of bound error"
  | otherwise = return $ String $ [s !! k']
  where k' = fromIntegral k
stringRef [(String s), notNum] = throwE $ TypeMismatch "number" notNum
stringRef [notString, _] = throwE $ TypeMismatch "string" notString
stringRef badArgList = throwE $ NumArgs 2 badArgList

car :: [LispVal] -> ThrowsException LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwE $ TypeMismatch "pair" badArg
car badArgList              = throwE $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsException LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwE $ TypeMismatch "pair" badArg
cdr badArgList              = throwE $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsException LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgsList              = throwE $ NumArgs 2 badArgsList

ifE :: [LispVal] -> ThrowsException LispVal
ifE [pred, conseq, alt] = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    Bool True  -> eval conseq
    _          -> throwE $ TypeMismatch "bool" pred
ifE notIf = throwE $ NumArgs 3 notIf

cond :: [LispVal] -> ThrowsException LispVal
cond ((List (Atom "else" : value : [])) : []) = eval value
cond ((List (condition : value : [])): alts) = do
  result <- eval condition
  boolResult <- unpackBool result
  if boolResult
    then eval value
    else cond alts
cond ((List a) : _) = throwE $ NumArgs 2 a
cond (a : _) = throwE $ NumArgs 2 [a]
cond _ = throwE $ Default "Not viable alternative in cond"

caseE :: [LispVal] -> ThrowsException LispVal
caseE form@(key : clauses) =
  if null clauses
  then throwE $ BadSpecialForm "no true clause in case expression: " (List (Atom "case" : form))
  else case head clauses of
      List (Atom "else" : exprs) -> mapM eval exprs >>= return . last
      List ((List datums) : exprs) -> do
        result <- eval key
        equality <- mapM (\x -> eqv [result, x]) datums
        if Bool True `elem` equality
          then mapM eval exprs >>= return . last
          else caseE $ key : tail clauses
      List (pred : exprs) -> do
        result <- eval key
        predResult <- eval pred
        equality <- eqv [result, predResult]
        if Bool True == equality
          then mapM eval exprs >>= return . last
          else caseE $ key : tail clauses
      _ -> throwE $ BadSpecialForm "ill-formed case expression: " (List (Atom "case" : form))
caseE form = throwE $ NumArgs 2 form

eqvList :: ([LispVal] -> ThrowsException LispVal) -> [LispVal] -> ThrowsException LispVal
eqvList eqvFunc [(List arg1), (List arg2)] =
  return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
  where
    eqvPair (x1, x2) = case runExcept $ eqvFunc [x1, x2] of
      Right (Bool val) -> val
      _                -> False
eqvList _ _ = return $ Bool False

eqv :: [LispVal] -> ThrowsException LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] =
  eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [l1@(List _), l2@(List _)] = eqvList eqv [l1, l2]
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwE $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsException LispVal
equal [l1@(List arg1), l2@(List arg2)] = eqvList equal [l1, l2]
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                      [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwE $ NumArgs 2 badArgList
