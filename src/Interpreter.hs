module Interpreter where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import Types
import Parser

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
  ("def", ESpecialFn eDefine),
  ("let", ESpecialFn eLet)]

eval :: Expr -> EResult Expr
eval e@(EInt _) = return e
eval ENil = return ENil
eval (ESymbol str) = do 
  env <- get
  case M.lookup str env of
    Just x -> return x
    Nothing -> throwError $ "Symbol not found: " ++ str
eval (EPair e1 e2) = liftM2 EPair (eval e1) (eval e2)
eval e@(EFn _) = return e
eval e@(ESpecialFn _) = return e
eval e@(ELambda _ _) = return e
eval (EApp s@(ESymbol _) e) = do
  sym <- eval s
  eval $ EApp sym e
eval (EApp (EFn f) e) = eval e >>= f
eval (EApp (ESpecialFn f) e) = f e
eval (EApp (ELambda param body) e) = do
  e' <- eval e
  current <- get
  modify $ M.insert param e'
  res <- eval body
  put current
  return res
eval ex@(EApp f e) = do
  e' <- eval e
  f' <- eval f
  case f' of
    fn@(EFn _) -> eval $ EApp fn e'
    fn@(ESpecialFn _) -> eval $ EApp fn e'
    fn@(ELambda _ _) -> eval $ EApp fn e'
    _ -> throwError $ "Unknown: " ++ show ex ++ " " ++ show f'
eval (EThen e1 e2) = do
  current <- get
  (value, state') <- runEResult (eval e1) current
  case value of
    Left err -> throwError err
    Right _ -> do
      put state'
      eval e2

pairIntMap :: String -> (Integer -> Integer -> Integer) -> Expr -> EResult Expr
pairIntMap _ f (EPair (EInt x1) (EInt x2)) = return . EInt $ f x1 x2
pairIntMap str _ e = throwError $ str ++ " requires two integers - " ++ show e

eAdd :: Expr -> EResult Expr
eAdd = pairIntMap "Add" (+)

eMultiply :: Expr -> EResult Expr
eMultiply = pairIntMap "Multiply" (*)

eSubtract :: Expr -> EResult Expr
eSubtract = pairIntMap "Subtract" (-)

eDefine :: Expr -> EResult Expr
eDefine (EPair (ESymbol s) e) = do
  e' <- eval e
  modify $ M.insert s e'
  return e'
eDefine _ = throwError "Define requires (Symbol Expr)"

eIf :: Expr -> EResult Expr
eIf (EPair cond (EPair e1 e2)) = do
  cond' <- eval cond
  if cond' /= ENil then eval e1 else eval e2
eIf _ = throwError "If requires (Cond, (e1, e2))"

eEq :: Expr -> EResult Expr
eEq (EPair e1 e2) = do
  e1' <- eval e1
  e2' <- eval e2
  return $ if e1' == e2' then (EInt 1) else ENil
eEq _ = throwError "Eq requires Pair of two values"

ePrint :: Expr -> EResult Expr
ePrint e = do
  e' <- eval e
  liftIO $ putStrLn . show $ e'
  return e

eFst :: Expr -> EResult Expr
eFst (EPair e _) = return e
eFst ex = throwError $ "fst requires a Pair: " ++ show ex

eSnd :: Expr -> EResult Expr
eSnd (EPair _ e) = return e
eSnd ex = throwError $ "snd requires a Pair: " ++ show ex

eCons :: Expr -> EResult Expr
eCons e@(EPair _ _) = return e
eCons _ = throwError "Cons requires a Pair"

eLet :: Expr -> EResult Expr
eLet (EPair (ESymbol sym) (EPair argument rest)) = do
  current <- get
  argument' <- eval argument
  modify $ M.insert sym argument'
  rest' <- eval rest
  put current
  return rest'
eLet ex = throwError $ "Let requires symbol and then expr: " ++ show ex
 
compute :: String -> IO ()
compute str = do
  case parseAll str of
    Left err -> putStr err
    Right x -> do
      res <- fst <$> runEResult (eval x) builtIns
      print res
