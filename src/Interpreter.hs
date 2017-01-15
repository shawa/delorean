module Interpreter where
import Evaluate
import Statement
import Expression
import Prompt
import Util (dedup, pprint)
import System.IO

import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Except
import Data.Maybe
import Data.List


type Run a = StateT [(Statement, Env)] (ExceptT String IO) a

runRun run = runExceptT $ runStateT run [(Pass, Map.empty)]

set :: Name -> Val -> Statement -> Run ()
set varname val stmt = state $ \envs -> ( (), ((stmt, Map.insert varname val (snd $ head envs)):envs))
  -- this is a bit dirty, but it's good for now; each time a variable is updated,
  -- we push a whole new environment onto the ever-growing list of environments

ppop :: Run Statement
ppop = state $ \(se:ses) -> (fst se, ses)

push :: Statement -> Env -> Run ()
push stmt env = state $ \stmtEnvs -> ((), (stmt, env):stmtEnvs)

hist :: Name -> [Env] -> [Val]
hist n = mapMaybe (Map.lookup n)

exec :: Statement -> Run ()
exec (stmt1 :. stmt2) = do
  prompt stmt1
  exec stmt2
  -- the second 'statement' is always a huge compound statement
  -- 'Do you want to run the rest of the program' would be a silly
  --- question to ask, so we only trigger the prompt on the first one

exec stmt@(Assign varname expr) = do
  ((_, env):_) <- get
  Right val <- return $ runEval env $ eval expr
  set varname val stmt

exec stmt@(Print expr) = do
  ((_,env):_) <- get
  Right val <- return $ runEval env $ eval expr
  liftIO $ print val
  push stmt env

exec stmt@(If expr stmt1 stmt2) = do
  ((_,env):_) <- get
  Right (B val) <- return $ runEval env $ eval expr
  if val then prompt stmt1 else prompt stmt2
  push stmt env

exec stmt@(While expr body) = do
  ((_,env):_) <- get
  Right (B val) <- return $ runEval env $ eval expr
  when val $ prompt body >> exec stmt
  push stmt env

exec (Try tryblock catchblock) = catchError (exec tryblock) (\_ -> exec catchblock)
exec Pass = return ()

prompt :: Statement -> Run ()
prompt stmt = do
  liftIO $ putStr "delorean> " >>  hFlush stdout
  cmd <- liftIO getLine
  case parseInput cmd of
    Step -> exec stmt

    Back -> do
      next <- ppop
      prompt next
      prompt stmt

    Dump -> do
      life <- get
      liftIO $ print life
      prompt stmt

    Changes -> do
      envs <- get
      liftIO $ mapM_ (putStrLn . pprint . snd) envs
      prompt stmt

    Inspect varname -> do
      stmtEnvs <- get
      let envs = map snd stmtEnvs
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
