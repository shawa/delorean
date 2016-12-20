module Interpreter where
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Maybe

import qualified Data.Map               as Map


type Name = String

data Exp = Lit Integer
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
         deriving (Show)

data Value = IntVal Integer
           | FunVal Env Name Exp
           deriving (Show)

type Env = Map.Map Name Value

type Eval a = ExceptT String Identity a

runEval :: Eval a -> Either String a
runEval ev = runIdentity (runExceptT ev)

eval :: Env -> Exp -> Eval Value
eval   _ (Lit i) = return $ IntVal i
eval env (Var n) = return $ fromJust $ Map.lookup n env

eval env (Plus e1 e2) = do e1' <- eval env e1
                           e2' <- eval env e2
                           case (e1', e2') of (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                                              _                      -> throwError "type error"

eval env (Abs n e) = return $ FunVal env n e
eval env (App e1 e2) = do val1 <- eval env e1
                          val2 <- eval env e2
                          case val1 of FunVal env' n body -> eval (Map.insert n val2 env') body
                                       _                  -> throwError "type error"
