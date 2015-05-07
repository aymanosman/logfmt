{-# LANGUAGE OverloadedStrings #-}
module Logfmt where

import Prelude hiding (putStrLn)
import Control.Applicative
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import Data.Text (Text)
import Data.Aeson
import Data.ByteString.Lazy.Char8 (putStrLn)

parseLine :: Parser [(Text, Maybe Text)]
parseLine =  many (P.skipSpace *> parsePair)
parsePair = (,) <$> parseKey <*> parseValue

parseKey = do
  k <- P.takeWhile (P.notInClass "=\" ")
  case k of
    "" -> fail ("Cannot have empty key")
    x -> return x

parseValue = do
  c <- P.peekChar
  case c of
    Nothing -> return $ Nothing
    Just ' ' -> return $ Nothing
    Just '=' -> P.char '=' >> parseVal
  where
    parseVal = do
      c <- P.peekChar
      case c of
        Nothing -> return $ Just ""
        Just '"' -> parseQuoted
        _ -> parseUnquoted

parseQuoted = P.char '"' >> s <* P.char '"'
  where
    s = Just <$> P.takeWhile (P.notInClass "\"")
parseUnquoted = Just <$> P.takeWhile (P.notInClass " ")

-- testing
parseToJson = encode <$> object <$> (fmap handlePair) <$> parseLine
  where
    handlePair (k, v) =
      case v of
        Nothing -> (k, Bool True)
        Just s -> (k, String s)

g a = let Right s = P.parseOnly parseToJson a in putStrLn s


--	ident_byte = any byte greater than ' ', excluding '=' and '"'
--	string_byte = any byte excluding '"' and '\'
--	garbage = !ident_byte
--	ident = ident_byte, { ident byte }
--	key = ident
--	value = ident | '"', { string_byte | '\', '"' }, '"'
--	pair = key, '=', value | key, '=' | key
--	message = { garbage, pair }, garbage
