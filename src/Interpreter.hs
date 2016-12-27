module Interpreter where
import Evaluate
import Statement
import Expression
import Prompt
import System.IO

import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Except
import Data.Maybe
import Data.List

type Run a = StateT [Env] (ExceptT String IO) a

runRun run = runExceptT $ runStateT run [Map.empty]

set :: Name -> Val -> Run ()
set varname val = state $ \envs -> ( (), (Map.insert varname val (head envs)):envs)
  -- this is a bit dirty, but it's good for now; each time a variable is updated,
  -- we push a whole new environment onto the ever-growing list of environments

hist :: Name -> [Env] -> [Val]
hist n envs = mapMaybe (Map.lookup n) envs

exec :: Statement -> Run ()
exec (stmt1 :. stmt2) = do
  prompt stmt1
  exec stmt2
  -- the second 'statement' is always a huge compound statement
  -- 'Do you want to run the rest of the program' would be a silly
  --- question to ask, so we only trigger the prompt on the first one

exec (Assign varname expr) = do
  (env:_) <- get
  Right val <- return $ runEval env $ eval expr
  set varname val

exec (Print expr) = do
  (env:_) <- get
  Right val <- return $ runEval env $ eval expr
  liftIO $ print val

exec (If expr stmt1 stmt2) = do
  (env: _) <- get
  Right (B val) <- return $ runEval env $ eval expr
  if val then prompt stmt1 else prompt stmt2

exec line@(While expr stmt) = do
  (env:_) <- get
  Right (B val) <- return $ runEval env $ eval expr
  if val
  then prompt stmt >> exec line
  else return ()

exec (Try tryblock catchblock) = do
  catchError (exec tryblock) (\_ -> exec catchblock)
  
exec Pass = return ()

prompt :: Statement -> Run ()
prompt stmt = do
  liftIO $ do
    putStr "delorean> " >>  hFlush stdout
  cmd <- liftIO getLine
  case parseInput cmd of
    Step -> exec stmt

    Dump -> do
      envs <- get
      liftIO $ print envs
      prompt stmt

    Inspect varname -> do
      envs <- get
      liftIO $ putStrLn $ case hist varname envs of
        [] -> "Unknown variable `"++ varname ++ "`"
        xs -> intercalate "\n" $ map show $ dedup xs
      prompt stmt

    Help -> do
      liftIO $ putStrLn helpString
      prompt stmt

    List -> do
      liftIO $ print stmt
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

dedup :: Eq a => [a] -> [a]
dedup []  = []
dedup [x] = [x]
dedup (x:y:xs) | x == y    = dedup (y:xs)
               | otherwise = x:y:(dedup xs)
