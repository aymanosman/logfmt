{-# LANGUAGE OverloadedStrings #-}
module Logfmt where

import Prelude hiding (putStrLn)
import Control.Monad
import Control.Applicative
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import Data.Text
import Data.Aeson as J
import Data.ByteString.Lazy.Char8 as L

-- parseLine :: Parser
parseLine =  many (P.skipSpace *> parsePair)
parsePair = (,) <$> parseKey <*> parseValue

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
toJsonString = encode <$> object <$> parseLine

--	ident_byte = any byte greater than ' ', excluding '=' and '"'
--	string_byte = any byte excluding '"' and '\'
--	garbage = !ident_byte
--	ident = ident_byte, { ident byte }
--	key = ident
--	value = ident | '"', { string_byte | '\', '"' }, '"'
--	pair = key, '=', value | key, '=' | key
--	message = { garbage, pair }, garbage
