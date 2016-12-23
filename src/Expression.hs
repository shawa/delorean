module Expression where

import qualified Data.Map as Map
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer


data Val = I Int | B Bool
           deriving (Eq, Read)

instance Show Val where
  show (I v) = show v ++ " : " ++ "Int"
  show (B v) = show v ++ " : " ++ "Bool"

data Expr = Const Val
          | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
          | And Expr Expr | Or  Expr Expr | Not Expr
          | Eq  Expr Expr | Gt  Expr Expr | Lt  Expr Expr
          | Var String
          deriving (Eq, Read)


instance Show Expr where
  show (Const val) = show val
  show (Add e1 e2) = show e1 ++ " + " ++ show e2
  show (Sub e1 e2) = show e1 ++ " - " ++ show e2
  show (Mul e1 e2) = show e1 ++ " * " ++ show e2
  show (Div e1 e2) = show e1 ++ " / " ++ show e2
  show (And e1 e2) = show e1 ++ " ⋀ " ++ show e2
  show (Or  e1 e2) = show e1 ++ " ⋁ " ++ show e2
  show (Not e1   ) = " ￢" ++ show e1
  show (Eq  e1 e2) = show e1 ++ " = " ++ show e2
  show (Gt  e1 e2) = show e1 ++ " > " ++ show e2
  show (Lt  e1 e2) = show e1 ++ " < " ++ show e2
  show (Var str  ) = str

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
