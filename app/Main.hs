{-#LANGUAGE QuasiQuotes#-}
import Interpreter (execute, Statement)
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
  programsource <- reaFile infile
  execute $ parse programsource
