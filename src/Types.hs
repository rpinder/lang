{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

type Context = M.Map String Expr

newtype EResult a = EResult { getEResult :: ExceptT String (StateT Context IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState Context, MonadError String)

runEResult :: MonadIO m => EResult a -> Context -> m (Either String a, Context)
runEResult (EResult x) ctx = liftIO $ runStateT (runExceptT x) ctx

data Expr = EInt Integer
          | ESymbol String
          | EPair Expr Expr
          | EFn (Expr -> EResult Expr)
          | ELambda String Expr
          | ESpecialFn (Expr -> EResult Expr)
          | EApp Expr Expr
          | EThen Expr Expr
          | ENil

instance Show Expr where
  show (EInt x) = show x
  show (ESymbol x) = x
  show (EPair x1 x2) = "(" ++ show x1 ++ "," ++ show x2 ++ ")"
  show (EFn _) = "<Fn>"
  show (ELambda s e) = "<" ++ show s ++ " : " ++ show e ++ ">"
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
