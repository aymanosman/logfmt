{-# LANGUAGE OverloadedStrings #-}

module Logfmt where

import Control.Monad
import Control.Applicative
import Data.Attoparsec.ByteString as P
import Data.Attoparsec.ByteString.Char8 as C
import qualified Data.ByteString.Char8 as K


p1 = P.takeWhile (P.notInClass "=\"")
p2 = p1 <* char8 '='

data Pair = Pair String String deriving (Show)

parsePair :: Parser Pair
parsePair = liftA2 Pair (fmap K.unpack p2) (fmap K.unpack parseVal)

parseVal = P.takeWhile (P.notInClass " ")

p3 = (parsePair <* ws) <* parsePair

ws = P.takeWhile (P.inClass " ")

finalP :: Parser [Pair]
finalP = do
  p1 <- many (parsePair <* skipWhitespace)
  return $ p1
  where
    skipWhitespace = ws

s = "key=value foo=\"a bar\""
s2 = "key=\"value\" foo=bar"
s3 = "key=\"a value\" foo=bar"
s4 = "a=1 b=2 c=3"

main = do
  -- p p1 s
  -- p p2 s
  -- p parsePair s
  -- p parsePair s3
  -- p p3 s
  p finalP s4
  p finalP s3
  where
    p pr s = do
      putStrLn (K.unpack s)
      case (parse pr s) of
        Done i r -> do
          print r
          putStrLn (K.unpack i)
        x -> do
          print "FAILURE"
          print x
      putStrLn ""


