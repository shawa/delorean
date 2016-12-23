{-#LANGUAGE QuasiQuotes#-}
import Control.Monad (when)
import Data.Char (toUpper)
import System.Environment (getArgs)

import System.Console.Docopt

import Statement
import Interpreter

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
  runRun $ prompt $ parse programsource
  return ()
