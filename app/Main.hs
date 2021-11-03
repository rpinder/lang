{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import qualified Data.Map as M
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Control.Monad.State
import Control.Monad.Except
import qualified Control.Exception as E

import System.IO

type Context = M.Map String Expr
type EResult = ExceptT String (StateT Context IO) Expr

data Expr = EInt Integer
          | ESymbol String
          | EPair Expr Expr
          | EFn (Expr -> EResult)
          | ELambda String Expr
          | ESpecialFn (Expr -> EResult)
          | EApp Expr Expr
          | EThen Expr Expr
          | ENil

instance Show Expr where
  show (EInt x) = show x
  show (ESymbol x) = x
  show (EPair x1 x2) = "(" ++ show x1 ++ "," ++ show x2 ++ ")"
  show (EFn _) = "<Fn>"
  show (ELambda _ _) = "<Lambda>"
  show (ESpecialFn _) = "<SpecialFn>"
  show (EApp e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (EThen e1 e2) = show e1 ++ "\n" ++ show e2
  show ENil = "nil"

instance Eq Expr where
  (EInt x) == (EInt y) = x == y
  (ESymbol x) == (ESymbol y) = x == y
  (EPair x1 x2) == (EPair y1 y2) = x1 == y1 && x2 == y2 
  ENil == ENil = True
  _ == _ = False

builtIns :: Context
builtIns = M.fromList [
  ("+", EFn eAdd),
  ("*", EFn eMultiply),
  ("-", EFn eSubtract),
  ("nil", ENil),
  ("if", ESpecialFn eIf),
  ("=", EFn eEq),
  ("fst", EFn eFst),
  ("snd", EFn eSnd),
  ("print", EFn ePrint),
  ("cons", EFn eCons),
  ("def", ESpecialFn eDefine)]

eval :: Expr -> EResult
eval (EInt x) = return $ EInt x
eval ENil = return $ ENil
eval (ESymbol str) = do 
  env <- lift $ get
  case M.lookup str env of
    Just x -> return x
    Nothing -> throwError $ "Symbol not found: " ++ str
eval (EPair e1 e2) = liftM2 EPair (eval e1) (eval e2)
eval (EFn f) = return $ EFn f
eval (ESpecialFn f) = return $ ESpecialFn f
eval e@(ELambda _ _) = return e
eval (EApp s@(ESymbol _) e) = do
  sym <- eval s
  eval $ EApp sym e
eval (EApp (EFn f) e) = eval e >>= f
eval (EApp (ESpecialFn f) e) = f e
eval (EApp l@(ELambda param body) e) = do
  e' <- eval e
  current <- lift get
  lift $ modify $ M.insert param e'
  res <- eval body
  lift $ put current
  return res
eval ex@(EApp f e) = do
  f' <- eval f
  case f' of
    fn@(EFn _) -> eval $ EApp fn e
    fn@(ESpecialFn _) -> eval $ EApp fn e
    fn@(ELambda _ _) -> eval $ EApp fn e
    _ -> throwError $ "Unknown: " ++ show ex ++ " " ++ show f'
eval (EThen e1 e2) = do
  current <- lift $ get
  (value, state') <- lift . lift $ runStateT (runExceptT $ eval e1) current
  case value of
    Left err -> throwError err
    Right _ -> do
      lift $ put state'
      eval e2

pairIntMap :: String -> (Integer -> Integer -> Integer) -> Expr -> EResult
pairIntMap _ f (EPair (EInt x1) (EInt x2)) = return . EInt $ f x1 x2
pairIntMap str _ e = throwError $ str ++ " requires two integers - " ++ show e

eAdd :: Expr -> EResult
eAdd = pairIntMap "Add" (+)

eMultiply :: Expr -> EResult
eMultiply = pairIntMap "Multiply" (*)

eSubtract :: Expr -> EResult
eSubtract = pairIntMap "Subtract" (-)

eDefine :: Expr -> EResult
eDefine (EPair (ESymbol s) e) = do
  e' <- eval e
  lift $ modify $ M.insert s e'
  return e'
eDefine _ = throwError "Define requires (Symbol Expr)"

eIf :: Expr -> EResult
eIf (EPair cond (EPair e1 e2)) = do
  cond' <- eval cond
  if cond' /= ENil then eval e1 else eval e2
eIf _ = throwError "If requires (Cond, (e1, e2))"

eEq :: Expr -> EResult
eEq (EPair e1 e2) = do
  e1' <- eval e1
  e2' <- eval e2
  return $ if e1' == e2' then (EInt 1) else ENil
eEq _ = throwError "Eq requires Pair of two values"

ePrint :: Expr -> EResult
ePrint e = do
  e' <- eval e
  lift . lift . putStrLn . show $ e'
  return e

eFst :: Expr -> EResult
eFst (EPair e _) = return e
eFst ex = throwError $ "fst requires a Pair: " ++ show ex

eSnd :: Expr -> EResult
eSnd (EPair _ e) = return e
eSnd ex = throwError $ "snd requires a Pair: " ++ show ex

eCons :: Expr -> EResult
eCons e@(EPair _ _) = return e
eCons _ = throwError "Cons requires a Pair"
  
type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme L.decimal

-- parsePair :: Parser Expr
-- parsePair = do
--   void (lexeme . char $ '(')
--   first <- lexeme $ parseExpr
--   void (lexeme . char $ ',')
--   second <- lexeme $ parseExpr
--   void (lexeme . char $ ')')
--   return $ EPair first second

-- parsePairSimple :: Parser Expr
-- parsePairSimple = do
--   es <- lexeme $ some parseExpr
--   return $ listToPairs es

-- parseApp :: Parser Expr
-- parseApp = lexeme $ parseAppSymbol <|> parseAppLambda

-- parseAppLambda :: Parser Expr
-- parseAppLambda = do
--   void (lexeme . char $ '(')
--   lambda <- lexeme $ parseLambda
--   argument <- lexeme $ try parsePairSimple <|> parseExpr
--   void (lexeme . char $ ')')
--   return $ EApp lambda argument

-- parseAppSymbol :: Parser Expr
-- parseAppSymbol = do
--   void (lexeme . char $ '(')
--   name <- lexeme $ some (letterChar <|> oneOf ("+-*=" :: String))
--   argument <- lexeme $ try parsePairSimple <|> parseExpr
--   void (lexeme . char $ ')')
--   return $ EApp (ESymbol name) argument

parseSymbol :: Parser Expr
parseSymbol = do
  name <- lexeme . some $ letterChar <|> oneOf ("+-*=" :: String)
  return $ ESymbol name

parseInteger :: Parser Expr
parseInteger = do
  int <- toInteger <$> integer
  return $ EInt int

-- parseLambda :: Parser Expr
-- parseLambda = do
--   void (lexeme . char $ '(')
--   void . lexeme . string $ "fn"
--   param <- lexeme $ some letterChar
--   body <- lexeme $ parseExpr
--   void (lexeme . char $ ')')
--   return $ ELambda param body

-- parseExpr :: Parser Expr
-- parseExpr = try parsePair <|> try parseLambda <|> try parseApp <|> try parseInteger <|> try parseSymbol

parseBracketsHelper :: Parser Expr
parseBracketsHelper = do
  void (lexeme . char $ '(')
  es <- lexeme $ some parseExpr
  let pairs = listToPairs es
  void (lexeme . char $ ')')
  return pairs

parseBrackets :: Parser Expr
parseBrackets = do
  expr <- parseBracketsHelper
  case expr of
    (EPair (ESymbol "fn") (EPair (ESymbol param) body)) -> return $ ELambda param body
    (EPair f arguments) -> return $ EApp f arguments
    ex -> fail $ show ex

parseExpr :: Parser Expr
parseExpr = try parseSymbol <|> try parseInteger <|> try parseBrackets

combineExprs :: [Expr] -> Either String Expr
combineExprs [] = Left "No Exprs"
combineExprs [x] = Right x
combineExprs (x:xs) = combineExprs xs >>= Right . EThen x

listToPairs :: [Expr] -> Expr
listToPairs [x] = x
listToPairs (x:xs) = EPair x $ listToPairs xs

pairsToList :: Expr -> [Expr]
pairsToList (EPair x y) = x : pairsToList y
pairsToList ex = [ex]

parseAll :: String -> Either String Expr
parseAll str = do
  parsed <- case runParser (someTill parseExpr eof) "" (T.pack str) of
    Left err -> Left $ errorBundlePretty err
    Right x -> Right x
  res <- combineExprs parsed
  return res

compute :: String -> IO ()
compute str = do
  case parseAll str of
    Left err -> putStr err
    Right x -> do
      res <- evalStateT (runExceptT $ eval x) builtIns
      print res

readInput :: IO String
readInput = do
  putStr "REPL> "
  hFlush stdout
  getLine

output :: String -> IO ()
output str = do
  case parseAll str of
    Left err -> putStr err
    Right x -> do
      res <- evalStateT (runExceptT $ eval x) builtIns
      case res of
        Left err -> putStrLn err
        Right x' -> print x'

safeRead :: String -> IO (Either String String)
safeRead str = do
  res <- E.try $ readFile str
  case res of
    Left (ex::E.SomeException) -> return $ Left $ "Exception : " ++ show ex
    Right val -> return $ Right val

repl :: IO ()
repl = do
  input <- readInput
  case input of
    ":q" -> return ()
    ":?" -> putStrLn ":? -> help\n:q -> quit\n:read <FILE> -> read from file" >> repl
    ':':'r':'e':'a':'d':' ':file -> do
      res <- safeRead file
      case res of
        Left e -> putStrLn e >> repl
        Right val -> output val >> repl
    str -> output str >> repl
    

main :: IO ()
main = do
  putStrLn "Lang REPL, :? for help"
  repl
