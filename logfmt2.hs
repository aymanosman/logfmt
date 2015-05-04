{-# LANGUAGE OverloadedStrings #-}
module Logfmt where

import Control.Monad
import Control.Applicative
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.ByteString.Char8 as K

data Pair = Pair K.ByteString K.ByteString deriving (Show)

parsePair :: Parser Pair
parsePair = Pair <$> parseKey <*> parseValue

parseKey = do
  k <- P.takeWhile (P.notInClass "= ")
  case k of
    "" -> fail ("Cannot have empty key")
    x -> return x

parseValue = do
  c <- C.peekChar
  case c of
    Nothing -> return "true" -- fail "wow"
    Just ' ' -> return "true"
    Just '=' -> C.char '=' >> parseVal
  where
    parseVal = do
      c <- C.peekChar
      case c of
        Nothing -> return ""
        Just '"' -> parseQuoted
        _ -> parseUnquoted

parseQuoted = C.char '"' >> P.takeWhile (P.notInClass "\"") <* C.char '"'
parseUnquoted = P.takeWhile (P.notInClass " ")

parseLine :: Parser [Pair]
parseLine = many (C.skipSpace *> parsePair)

s = "key=value foo=\"a bar\""
s2 = "key=\"value\" foo=bar"
s3 = "key=\"a value\" foo=bar"
s4 = "a=1 b=2 c=3"
s5 = "schmoo=\"hello\nworld\""
s6 = "ip=127.0.0.1 time=2015-05-03T13:54:03.396Z method=GET path=\"/test/user/has-friend?friend=example\" status=304 content_length= content_type= elapsed=2ms"
s7 = "a=foo b=10ms c=cat E=\"123\" d foo= emp="
s8 = "measure.a=1ms measure.b=10 measure.c=100MB measure.d=1s garbage"
s9 = "foo=bar a=14 baz=\"hello kitty\" cool%story=bro f %^asdf"

ss = [s6, s7, s8, s9]

main = do
  forM_ ss (p parseLine)
  where
  p pr s = do
    K.putStrLn s
    let Right r = P.parseOnly pr s
    mapM_ print r
    putStrLn ""
