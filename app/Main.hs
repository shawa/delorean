{-#LANGUAGE QuasiQuotes#-}
import Control.Monad (when)
import Data.Char (toUpper)
import System.Environment (getArgs)

import System.Console.Docopt
import Data.List (intersperse)
import Data.List.Utils (replace)
import Statement
import Interpreter

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

parse :: String -> Statement
parse string = read (replace "\n" " " string)


banner :: String
banner = concat $ intersperse "\n" lines
         where lines = [ "________     ______                                 "
                       , "___  __ \\_______  /________________________ _______ "
                       , "__  / / /  _ \\_  / _  __ \\_  ___/  _ \\  __ `/_  __ \\"
                       , "_  /_/ //  __/  /__/ /_/ /  /   /  __/ /_/ /_  / / /"
                       , "/_____/ \\___//_____|____//_/    \\___/\\__,_/ /_/ /_/ "
                       , "The Amazing Time-Travelling Interpreter!              "
                       , "type `help` or `h` for a list of commands             "
                       , ""]
main = do
  args <- parseArgsOrExit patterns =<< getArgs
  infile <- args `getArgOrExit` (argument "infile")
  programsource <- readFile infile
  putStrLn banner
  runRun $ prompt $! parse programsource
  return ()
