{-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Interpreter where

import qualified Data.Map as Map
import Data.Maybe

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer


data Val = I Int | B Bool
           deriving (Eq, Show)

data Expr = Const Val
     | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
     | And Expr Expr | Or  Expr Expr | Not Expr
     | Eq  Expr Expr | Gt  Expr Expr | Lt  Expr Expr
     | Var String
   deriving (Eq, Show)

type Name = String
type Env = Map.Map Name Val


type Eval a = ReaderT Env (ExceptT String Identity) a
runEval env ex = runIdentity ( runExceptT ( runReaderT ex env) )

evali op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (I i0, I i1) -> return $ I (i0 `op` i1)
                         _            -> fail "type error in arithmetic expression"

evalb op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (B i0, B i1) -> return $ B (i0 `op` i1)
                         _            -> fail "type error in boolean expression"

evalib op e0 e1 = do e0' <- eval e0
                     e1' <- eval e1
                     case (e0', e1') of
                          (I i0, I i1) -> return $ B (i0 `op` i1)
                          _            -> fail "type error in arithmetic expression"

--Evaluate an expression

eval :: Expr -> Eval Val
eval (Const v) = return v
eval (Add e0 e1) = do evali (+) e0 e1
eval (Sub e0 e1) = do evali (-) e0 e1
eval (Mul e0 e1) = do evali (*) e0 e1
eval (Div e0 e1) = do evali div e0 e1

eval (And e0 e1) = do evalb (&&) e0 e1
eval (Or e0 e1) = do evalb (||) e0 e1

eval (Not e0  ) = do evalb (const not) e0 (Const (B True))

eval (Eq e0 e1) = do evalib (==) e0 e1
eval (Gt e0 e1) = do evalib (>) e0 e1
eval (Lt e0 e1) = do evalib (<) e0 e1

eval (Var s) = do env <- ask
                  resolve s env
                  where resolve varname table = case Map.lookup varname table of
                                                  Just x  -> return x
                                                  Nothing -> fail ("Unknown variable "++varname)

data Statement = Seq Statement Statement
               | Assign String Expr
               | Print Expr
               | If Expr Statement Statement
               | While Expr Statement
               | Try Statement Statement
               | Pass
      deriving (Eq, Show)

instance Monoid Statement where
  mempty = Pass
  mappend = Seq


set :: Name -> Val -> Runnable ()
set varname val = state $ \table -> ( (), Map.insert varname val table)


type Runnable a = StateT Env (ExceptT String IO) a
runRunnable runnable = runExceptT $ runStateT runnable Map.empty

execute :: Statement -> Runnable ()
execute (Seq stmt1 stmt2) = execute stmt1 >> execute stmt2
execute (Assign varname expr) = do env <- get
                                   Right val <- return $ runEval env (eval expr)
                                   set varname val

execute (Print expr) = do env <- get
                          Right val <- return $ runEval env (eval expr)
                          liftIO $ print val

execute Pass = return ()
