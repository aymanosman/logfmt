{-# LANGUAGE OverloadedStrings #-}
module Logfmt where

import Control.Applicative
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import Data.Text (Text)

parseLine :: Parser [(Text, Maybe Text)]
parseLine =  many (P.skipSpace *> parsePair)

parsePair :: Parser (Text, Maybe Text)
parsePair = (,) <$> parseKey <*> parseValue

parseKey :: Parser Text
parseKey = P.takeWhile1 (P.notInClass "=\" ")

parseValue :: Parser (Maybe Text)
parseValue =
  do c <- P.peekChar
     case c of
       Just '=' -> P.char '=' >> val
       _ -> return Nothing
  where
    val = do
      c <- P.peekChar
      case c of
        Nothing -> return $ Just ""
        Just '"' -> parseQuoted
        _ -> parseUnquoted

parseUnquoted :: Parser (Maybe Text)
parseUnquoted = Just <$> P.takeWhile (P.notInClass " \"")

parseQuoted :: Parser (Maybe Text)
parseQuoted = P.char '"' *> s <* P.char '"'
  where
    s = Just . mconcat <$> many (esc <|> stringBytes)
    esc = P.string "\\\""
    stringBytes = P.takeWhile1 (P.notInClass "\"\\")
