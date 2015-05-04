{-# LANGUAGE OverloadedStrings #-}
module Logfmt where

import Control.Monad
import Control.Applicative
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import Data.Text
import Data.Aeson as J

-- parseLine :: Parser
parseLine =  many (P.skipSpace *> parsePair)

toJsonString = encode . object <$> parseLine

parsePair = (.=) <$> parseKey <*> parseValue

parseKey = do
  k <- P.takeWhile (P.notInClass "= ")
  case k of
    "" -> fail ("Cannot have empty key")
    x -> return x

parseValue = do
  c <- P.peekChar
  case c of
    Nothing -> return $ Bool True -- fail "wow"
    Just ' ' -> return $ Bool True
    Just '=' -> P.char '=' >> parseVal
  where
    parseVal = do
      c <- P.peekChar
      case c of
        Nothing -> return $ String ""
        Just '"' -> parseQuoted
        _ -> parseUnquoted

parseQuoted = P.char '"' >> s <* P.char '"'
  where
    s = String <$> P.takeWhile (P.notInClass "\"")
parseUnquoted = String <$> P.takeWhile (P.notInClass " ")

-- testing
g a = let Right x = P.parseOnly toJsonString a in L.putStrLn x

