{-# LANGUAGE FlexibleContexts #-}
module Prompt where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char

import Expression (Name)

data Command = Step
             | Inspect Name
             | Help
             | Undefined
             deriving (Show)

parseInput :: String -> Command
parseInput s = either (\_ -> Undefined) id $ parse parseCommand [] s
  
parseCommand :: Parser Command
parseCommand =
  parseStep <|> parseInspect <|> parseHelp
  where
    parseStep = do
      stringOrInitial "step"
      eof
      return Step

    parseInspect = do
      stringOrInitial "inspect"
      skipMany1 space
      varName <- many1 alphaNum
      eof
      return $ Inspect varName

    parseHelp = do
      stringOrInitial "help"
      eof
      return Help

    stringOrInitial str@(c:_) = try (string str) <|> string [c]
