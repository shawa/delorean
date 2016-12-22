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

exec :: Statement -> Runnable ()
exec (Seq stmt1 stmt2) = do execute stmt1
                            exec stmt2

exec (Assign varname expr) = do env <- get
                                Right val <- return $ runEval env $ eval expr
                                set varname val

exec (Print expr) = do env <- get
                       Right val <- return $ runEval env (eval expr)
                       liftIO $ print val

exec (If expr stmt1 stmt2) = do env <- get
                                Right (B val) <- return $ runEval env $ eval expr
                                if val then exec stmt1 else exec stmt2

exec (While expr stmt) = do env <- get
                            Right (B val) <- return $ runEval env $ eval expr
                            if val then exec stmt >> exec (While expr stmt) else return ()

exec (Try tryblock catchblock) = do catchError (exec tryblock) (\_ -> exec catchblock)

exec Pass = return ()

execute :: Statement -> Runnable ()
execute stmt = do liftIO $ print stmt
                  _ <- liftIO $ getLine
                  exec stmt
  
testprog = exec $ mconcat [ (Assign "a" (Const (I 50)))
                          , (Assign "b" (Const (I 20)))
                          , (Assign "c" (Add (Var "a") (Var "b")))
                          , (Print (Var "c"))
                          , (If (Gt (Var "b") (Var "a")) (Print (Var "b")) (Print (Var "a")))
                          ]
