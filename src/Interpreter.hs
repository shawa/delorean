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


-- I'll be calling this [(Statement, Env)] list the 'life stack', as it's a
-- stack which documents the life of the program up until the most recent
-- statement that's ben executed
type Run a = StateT [(Statement, Env)] (ExceptT String IO) a

runRun run = runExceptT $ runStateT run [(Pass, Map.empty)]

set :: Name -> Val -> Statement -> Run ()
set varname val stmt = state $ \envs -> ( (), ((stmt, Map.insert varname val (snd $ head envs)):envs))
  -- this is a bit dirty, but it's good for now; each time a variable is updated,
  -- we push a whole new environment onto the ever-growing list of environments

-- pop off the last (Statement, Env) pair and return the statment to execute
pop :: Run Statement
pop = state $ \(se:ses) -> (fst se, ses)

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

-- for the rest of these, we get the most recent env by popping it
-- as the right element off of the 'life' stack. We then do whatever
-- the statement says to do, then push the statement and its env back
-- on the life stack
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
      next <- pop
      prompt next
      prompt stmt

    Dump -> do
      life <- get
      liftIO $ print life
      prompt stmt

    -- grab all of the old envs
    Changes -> do
      envs <- get
      liftIO $ mapM_ (putStrLn . pprint . snd) envs
      prompt stmt
      where pprint e = intercalate ", " $ map (\(k, v) -> k ++ " -> " ++ show v) $ Map.toList e


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


-- why is this not in Data.List
-- when we're showing the history of a variable, we don't want to print out that for 5
-- consecutive lines of the program a variable's value was unchanged
dedup :: Eq a => [a] -> [a]
dedup []  = []
dedup [x] = [x]
dedup (x:y:xs) | x == y    = dedup (y:xs)
               | otherwise = x:y:dedup xs

helpString :: String
helpString = "  Available commands: \n\
             \    h help    : Print this message\n\
             \    s step    : Execute one statement of the program\n\
             \    b back    : Undo one statement of the program \n\
             \    l list    : List the current statement\n\
             \    c changes : List all different states of the program\n\
             \    d dump    : (debug) Dump out all runtime data\n\
             \\
             \    i inspect <variable name>:\n\
             \         Inspect given variable's content\n"
