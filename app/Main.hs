{-#LANGUAGE QuasiQuotes#-}
import Interpreter 
import Control.Monad (when)
import Data.Char (toUpper)
import System.Environment (getArgs)

import System.Console.Docopt

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

parse :: String -> Statement
parse string = let statements = read string :: [Statement]
               in mconcat statements

main = do
  args <- parseArgsOrExit patterns =<< getArgs
  infile <- args `getArgOrExit` (argument "infile")
  programsource <- readFile infile
  runRunnable $ exec $ parse programsource
  return ()

-- main monadic executer thing with a stateT of the env
-- but one secret trick! the env is really zipper over a list of previous/future envs
