{-# LANGUAGE OverloadedStrings #-}

module Logfmt where

import Control.Monad
import Control.Applicative
import qualified Data.ByteString as B
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
    parseUnquoted :: Parser B.ByteString
    parseUnquoted = P.takeWhile (P.notInClass " ")
    parseQuoted :: Parser K.ByteString
    parseQuoted = do
      char '"'
      v <- P.takeWhile (P.notInClass "\"") <* char '"'
      return v

ws = P.takeWhile (P.inClass " ")

parseLine :: Parser [Pair]
parseLine = (many parsePair) <* endOfLine

s = "key=value foo=\"a bar\"\n"
s2 = "key=\"value\" foo=bar\n"
s3 = "key=\"a value\" foo=bar\n"
s4 = "a=1 b=2 c=3\n"

main = do
  p parseLine s
  p parseLine s2
  p parseLine s3
  p parseLine s4
  where
    p pr s = do
      putStrLn (K.unpack s)
      case (parseOnly pr s) of
        Right r -> do
          print r
          -- putStrLn (K.unpack i)
        x -> do
          print "FAILURE"
          print x
      putStrLn ""


