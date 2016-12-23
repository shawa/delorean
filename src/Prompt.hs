{-# LANGUAGE FlexibleContexts #-}
module Prompt where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char

import Expression (Name)

data Command = Step
             | Dump
             | Help
             | Inspect Name
             | Undefined
             deriving (Show)

parseInput :: String -> Command
parseInput s = either (\_ -> Undefined) id $ parse parseCommand [] s

parseCommand :: Parser Command
parseCommand =  parseStep
            <|> parseDump
            <|> parseHelp
            <|> parseInspect
  where

    parseStep    = parseNullary "step"    Step
    parseDump    = parseNullary "dump"    Dump
    parseHelp    = parseNullary "help"    Help
    parseInspect = parseUnary   "inspect" Inspect

    parseNullary cmdName cmd = stringOrInitial cmdName >> eof >> return cmd
    parseUnary   cmdName cmd = do
      stringOrInitial cmdName
      skipMany1 space
      argValue <- many1 alphaNum
      eof
      return $ cmd argValue

    stringOrInitial str@(c:_) = try (string str) <|> string [c]
    -- this way, the user can call a command by typing either the
    -- whole thing, or just it's first character
