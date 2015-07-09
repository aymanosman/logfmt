{-# LANGUAGE OverloadedStrings #-}
module Logfmt where

import Control.Applicative
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import Data.Monoid
import Data.Text (Text)

parseLine :: Parser [(Text, Maybe Text)]
parseLine =  many (P.skipSpace *> parsePair)
parsePair = (,) <$> parseKey <*> parseValue

parseKey = P.takeWhile1 (P.notInClass "=\" ")

parseValue = do
  c <- P.peekChar
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

parseUnquoted = Just <$> P.takeWhile (P.notInClass " \"")
parseQuoted = P.char '"' *> s <* P.char '"'
  where
    s = Just <$> mconcat <$> many (esc <|> stringBytes)
    esc = P.string "\\\""
    stringBytes = P.takeWhile1 (P.notInClass "\"\\")
