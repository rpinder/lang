module Main where

{-# LANGUAGE OverloadedStrings #-}

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

type Context = M.Map String Expr
type EResult = ExceptT String (StateT Context IO) Expr

data Expr = EInt Integer
          | ESymbol String
          | EPair Expr Expr
          | EFn (Expr -> EResult)
          | ESpecialFn (Expr -> EResult)
          | EApp Expr Expr
          | EThen Expr Expr

instance Show Expr where
  show (EInt x) = show x
  show (ESymbol x) = x
  show (EPair x1 x2) = "(" ++ show x1 ++ "," ++ show x2 ++ ")"
  show (EFn _) = "<Fn>"
  show (ESpecialFn _) = "<SpecialFn>"
  show (EApp e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (EThen e1 e2) = show e1 ++ "\n" ++ show e2

builtIns :: Context
builtIns = M.fromList [
  ("+", EFn eAdd),
  ("*", EFn eMultiply),
  ("-", EFn eSubtract),
  ("define", ESpecialFn eDefine)]

eval :: Expr -> EResult
eval (EInt x) = return $ EInt x
eval (ESymbol str) = do 
  env <- lift $ get
  case M.lookup str env of
    Just x -> return x
    Nothing -> throwError $ "Symbol not found: " ++ str
eval (EPair e1 e2) = liftM2 EPair (eval e1) (eval e2)
eval (EFn f) = return $ EFn f
eval (ESpecialFn f) = return $ ESpecialFn f
eval (EApp s@(ESymbol _) e) = do
  sym <- eval s
  eval $ EApp sym e
eval (EApp (EFn f) e) = eval e >>= f
eval (EApp (ESpecialFn f) e) = f e
eval (EThen e1 e2) = do
  current <- lift $ get
  state' <- lift . lift $ execStateT (runExceptT $ eval e1) current
  lift $ put state'
  eval e2
eval e = throwError $ "Unknown: " ++ show e

pairIntMap :: String -> (Integer -> Integer -> Integer) -> Expr -> EResult
pairIntMap _ f (EPair (EInt x1) (EInt x2)) = return . EInt $ f x1 x2
pairIntMap str _ _ = throwError $ str ++ " requires two integers"

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

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme L.decimal

parsePair :: Parser Expr
parsePair = do
  void (lexeme . char $ '(')
  first <- parseExpr
  void (lexeme . char $ ',')
  second <- parseExpr
  void (char ')')
  return $ EPair first second

parseApp :: Parser Expr
parseApp = parseAppSymbol

parseAppSymbol :: Parser Expr
parseAppSymbol = do
  void (lexeme . char $ '(')
  name <- lexeme $ some (alphaNumChar <|> oneOf "+-*/")
  argument <- parseExpr
  void (lexeme . char $ ')')
  return $ EApp (ESymbol name) argument

parseSymbol :: Parser Expr
parseSymbol = do
  name <- lexeme $ some alphaNumChar
  return $ ESymbol name

parseInteger :: Parser Expr
parseInteger = do
  int <- toInteger <$> integer
  return $ EInt int

parseExpr :: Parser Expr
parseExpr = try parsePair <|> try parseApp <|> try parseInteger <|> try parseSymbol

combineExprs :: [Expr] -> Either String Expr
combineExprs [] = Left "No Exprs"
combineExprs [x] = Right x
combineExprs (x:xs) = combineExprs xs >>= Right . EThen x

parseAll :: String -> Either String Expr
parseAll str = do
  parsed <- case runParser (some parseExpr) "" (T.pack str) of
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

main :: IO ()
main = print ""
