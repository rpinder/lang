{-# LANGUAGE ScopedTypeVariables #-}
module Repl where

import qualified Control.Exception as E
import System.IO

import Interpreter
import Types
import Parser

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
      res <- fst <$> runEResult (eval x) builtIns
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
 
