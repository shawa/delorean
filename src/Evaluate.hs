module Evaluate where

import qualified Data.Map as Map
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Identity (Identity, runIdentity)

import Expression (Expr(..), Name, Val(..))

type Env = Map.Map Name Val
type Eval a = ReaderT Env (ExceptT String Identity) a

runEval env ex = runIdentity ( runExceptT ( runReaderT ex env) )

evali op e0 e1 = do
  e0' <- eval e0
  e1' <- eval e1
  case (e0', e1') of
        (I i0, I i1) -> return $ I (i0 `op` i1)
        _            -> fail "type error in arithmetic expression"

evalb op e0 e1 = do
  e0' <- eval e0
  e1' <- eval e1
  case (e0', e1') of
        (B i0, B i1) -> return $ B (i0 `op` i1)
        _            -> fail "type error in boolean expression"

evalib op e0 e1 = do
  e0' <- eval e0
  e1' <- eval e1
  case (e0', e1') of
      (I i0, I i1) -> return $ B (i0 `op` i1)
      _            -> fail "type error in arithmetic expression"

eval :: Expr -> Eval Val
eval (Const v) = return v
eval (Add e0 e1) = evali (+) e0 e1
eval (Sub e0 e1) = evali (-) e0 e1
eval (Mul e0 e1) = evali (*) e0 e1
eval (Div e0 e1) = evali div e0 e1
eval (And e0 e1) = evalb (&&) e0 e1
eval (Or  e0 e1) = evalb (||) e0 e1
eval (Not e0   ) = evalb (const not) e0 (Const (B True))
eval (Eq  e0 e1) = evalib (==) e0 e1
eval (Gt  e0 e1) = evalib (>) e0 e1
eval (Lt  e0 e1) = evalib (<) e0 e1

eval (Var name) = do
  env <- ask
  case Map.lookup name env of
        Just x  -> return x
        Nothing -> fail ("Unknown variable "++ name)
