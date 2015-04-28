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

s = "key=value foo=\"a bar\""
test = parse p1 s
test2 = parse p2 s
test3 = P.parse parsePair s
s2 = "key=\"value\" foo=bar"


s3 = "key=\"a value\" foo=bar"


main = do
  p p1 s
  p p2 s
  -- p (K.pack . show . parsePair) s
  where
    p pr s = do
      putStr (K.unpack s)
      case (parse pr s) of
        Done i r -> putStr (" -> " ++ (K.unpack r) ++ " <- " ++ (K.unpack i))
        _ -> fail "woops"
      putStrLn ""


