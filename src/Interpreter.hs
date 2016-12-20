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

type Eval a = ReaderT Env (ExceptT String (StateT Integer Identity )) a

runEval :: Env -> Integer -> Eval a -> (Either String a, Integer)
runEval env st ev = runIdentity (runStateT ( runExceptT ( runReaderT ev env)) st)

tick :: (Num s, MonadState s m ) => m ()
tick = get >>= \st -> put $ st + 1

  
eval :: Exp -> Eval Value
eval (Lit i)      = do tick
                       return $ IntVal i
eval (Var n)      = do tick
                       env <- ask
                       case Map.lookup n env of
                         Nothing  -> throwError ("unbound variable " ++ n)
                         Just val -> return val

eval (Plus e1 e2) = do tick
                       e1' <- eval e1
                       e2' <- eval e2
                       case (e1', e2') of
                         (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                         _                      -> throwError "type error"
eval (Abs n e)    = do tick
                       env <- ask
                       return $ FunVal env n e

eval (App e1 e2)  = do tick
                       val1 <- eval e1
                       val2 <- eval e2
                       case val1 of
                         FunVal env' n body -> local (const $ Map.insert n val2 env') (eval body)
                         _                  -> throwError "type error"

evaluate :: Eval a -> (Either String a, Integer)
evaluate = runEval Map.empty 0
