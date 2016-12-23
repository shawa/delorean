module Statement where

import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import System.IO

import Expression
import Prompt

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
runRun run = runExceptT $ runStateT run Map.empty

exec :: Statement -> Run ()
exec (Seq stmt1 stmt2) = do
  prompt stmt1
  exec stmt2
  -- the second 'statement' is always a huge compound statement
  -- 'Do you want to run the rest of the program' would be a silly
  --- question to ask, so we only trigger the prompt on the first one

exec (Assign varname expr) = do
  env <- get
  Right val <- return $ runEval env $ eval expr
  set varname val

exec (Print expr) = do
  env <- get
  Right val <- return $ runEval env $ eval expr
  liftIO $ print val

exec (If expr stmt1 stmt2) = do
  env <- get
  Right (B val) <- return $ runEval env $ eval expr
  if val then exec stmt1 else exec stmt2

exec (While expr stmt) = do
  env <- get
  Right (B val) <- return $ runEval env $ eval expr
  if val
  then exec stmt >> exec (While expr stmt)
  else return ()

exec (Try tryblock catchblock) = do
  catchError (exec tryblock) (\_ -> exec catchblock)

exec Pass = return ()

prompt :: Statement -> Run ()
prompt stmt = do
  liftIO $ putStr "delorean> " >>  hFlush stdout
  cmd <- liftIO getLine
  command <- return $ parseInput cmd

  case command of
    Step -> do
      exec stmt

    Dump -> do
      env <- get
      liftIO $ print env
      prompt stmt

    Inspect varname -> do
      env <- get
      var <- return $ Map.lookup varname env
      liftIO $ case var of Just val -> putStrLn $ varname ++ " " ++ (show val)
                           Nothing  -> putStrLn $ "Undefined variable `" ++ varname ++ "`"
      prompt stmt

    Help -> do
      liftIO $ putStrLn helpString
      prompt stmt

    List -> do
      liftIO $ do putStrLn $ ">>> " ++ (show stmt)
      prompt stmt

    Undefined -> do
      liftIO $ putStrLn $ "Unknown command `" ++ cmd ++ "`" ++
                          "\nType `help` or just `h` for a list of commands"
      prompt stmt


helpString :: String
helpString = "  Available commands: \n\
             \    h help: Print this message\n\
             \    s step: Execute one statement of the program\n\
             \    l list: List the current statement\n\
             \    d dump: Dump out the whole var table\n\
             \\
             \    i inspect <variable name>:\n\
             \         Inspect given variable's content\n"
