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
  char '='
  val <- K.unpack <$> pVal
  return $ Pair key val
  where
    pKey = P.takeWhile (P.notInClass "=")
    pVal = do
      Just c1 <- C.peekChar
      val <- if c1 == '"'
             then parseQuoted
             else parseUnquoted
      return val
    parseUnquoted = P.takeWhile (P.notInClass " ")
    parseQuoted = do
      char '"'
      v <- P.takeWhile (P.notInClass "\"") <* char '"'
      return v

ws = P.takeWhile (P.inClass " ")

parseLine :: Parser [Pair]
parseLine = (many parsePair)

s = "key=value foo=\"a bar\""
s2 = "key=\"value\" foo=bar"
s3 = "key=\"a value\" foo=bar"
s4 = "a=1 b=2 c=3"
s5 = "schmoo=\"hello\nworld\""

main = do
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


