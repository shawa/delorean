{-# LANGUAGE FlexibleContexts #-}
module Prompt where
-- A parser for the command line. You could probably do this
-- with regular expressions but this is more extensible.
-- It's also a nice excuse to use monadic parser combinators

import Text.Parsec
import Text.Parsec.String (Parser)

import Expression (Name)

data Command = Step
             | Dump
             | Help
             | List
             | Changes
             | Back
             | Inspect Name
             | Undefined
             deriving (Show)

parseInput :: String -> Command
parseInput = either (const Undefined) id . parse parseCommand []

parseCommand :: Parser Command
parseCommand =  parseNullary "step"    Step
            <|> parseNullary "dump"    Dump
            <|> parseNullary "help"    Help
            <|> parseNullary "list"    List
            <|> parseNullary "back"    Back
            <|> parseNullary "changes" Changes
            <|> parseUnary   "inspect" Inspect
  where
    parseNullary cmdName cmd = stringOrInitial cmdName >> eof >> return cmd

    parseUnary   cmdName cmd = do
      stringOrInitial cmdName
      skipMany1 space
      argValue <- many1 alphaNum
      eof
      return $ cmd argValue

    stringOrInitial str@(c:_) = try (string str) <|> string [c]
    -- this way, the user can call a command by typing either the
    -- whole thing, or just its first character
