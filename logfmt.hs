{-# LANGUAGE OverloadedStrings #-}
module Logfmt where

import Control.Monad
import Control.Applicative
import Data.Attoparsec.ByteString as P
import Data.Attoparsec.ByteString.Char8 as C
import qualified Data.ByteString.Char8 as K

data Pair = Pair String String deriving (Show)

parsePair :: Parser Pair
parsePair = do
  key <- K.unpack <$> (ws *> pKey)
  val <- K.unpack <$> pVal
  return $ Pair key val

pKey = P.takeWhile (P.notInClass "= ")
pVal = do
  -- c <- C.peekChar
  -- val <- case c of
  --   Nothing -> return "true"
  --   Just ' ' -> return "true"
  --   Just '=' -> return ""
  Just c <- C.peekChar
  case c of
    ' ' -> return "true"
    _ -> do char '=' >> pVal'

pVal' = do
  Just c1 <- C.peekChar
  case c1 of
    '"' -> parseQuoted
    _ -> parseUnquoted

parseUnquoted = P.takeWhile (P.notInClass " ")
parseQuoted = do
  char '"' >> P.takeWhile (P.notInClass "\"") <* char '"'

ws = P.takeWhile (P.inClass " ")

parseLine :: Parser [Pair]
parseLine = (many parsePair)

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
    let Right r = parseOnly pr s
    mapM_ print r
    putStrLn ""

main2 = do
  p parseLine s
  p parseLine s2
  p parseLine s3
  p parseLine s4
  p parseLine s5
  where
    p pr s = do
      putStrLn (K.unpack s)
      case (parseOnly pr s) of
        Right r -> print r
        x -> do
          print "FAILURE"
          print x
      putStrLn ""


