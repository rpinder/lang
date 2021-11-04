{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor (void)
import Data.Void

import Types

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme L.decimal

parseSymbol :: Parser Expr
parseSymbol = do
  name <- lexeme . some $ letterChar <|> oneOf ("+-*=" :: String)
  return $ ESymbol name

parseInteger :: Parser Expr
parseInteger = do
  int <- toInteger <$> integer
  return $ EInt int

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

parseList :: Parser Expr
parseList = do
  void (lexeme . char $ '[')
  es <- lexeme $ some parseExpr
  let pairs = listToConsList es
  void (lexeme . char $ ']')
  return pairs

parseExpr :: Parser Expr
parseExpr = try parseSymbol <|> try parseInteger <|> try parseBrackets <|> parseList

combineExprs :: [Expr] -> Either String Expr
combineExprs [] = Left "No Exprs"
combineExprs [x] = Right x
combineExprs (x:xs) = combineExprs xs >>= Right . EThen x

listToPairs :: [Expr] -> Expr
listToPairs [x] = x
listToPairs (x:xs) = EPair x $ listToPairs xs

listToConsList :: [Expr] -> Expr
listToConsList [] = ENil
listToConsList (x:xs) = EPair x $ listToConsList xs

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


