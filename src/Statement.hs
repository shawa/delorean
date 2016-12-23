module Statement where

import qualified Data.Map as Map

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Expression

data Statement = Seq Statement Statement
               | Assign String Expr
               | Print Expr
               | If Expr Statement Statement
               | While Expr Statement
               | Try Statement Statement
               | Pass
      deriving (Eq, Show, Read)

instance Monoid Statement where
  mempty = Pass
  mappend = Seq


set :: Name -> Val -> Run ()
set varname val = state $ \table -> ( (), Map.insert varname val table)


type Run a = StateT Env (ExceptT String IO) a
runRun runnable = runExceptT $ runStateT runnable Map.empty

exec :: Statement -> Run ()
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

execute :: Statement -> Run ()
execute stmt = do liftIO $ print stmt
                  _ <- liftIO getLine
                  exec stmt
