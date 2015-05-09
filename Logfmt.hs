{-# LANGUAGE OverloadedStrings #-}
module Logfmt where

import Prelude hiding (putStrLn)
import Control.Applicative
import Data.Monoid
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import Data.Text (Text)
import Data.Aeson
import Data.ByteString.Lazy.Char8 (putStrLn)

parseLine :: Parser [(Text, Maybe Text)]
parseLine =  many (P.skipSpace *> parsePair)
parsePair = (,) <$> parseKey <*> parseValue

parseKey = P.takeWhile1 (P.notInClass "=\" ")

parseValue = do
  c <- P.peekChar
  case c of
    Nothing -> return $ Nothing
    Just ' ' -> return $ Nothing
    Just '=' -> P.char '=' >> val
  where
    val = do
      c <- P.peekChar
      case c of
        Nothing -> return $ Just ""
        Just '"' -> parseQuoted
        _ -> parseUnquoted

parseUnquoted = Just <$> P.takeWhile (P.notInClass " ")
parseQuoted = P.char '"' *> s <* P.char '"'
  where
    -- s = do
    --   Just c <- P.peekChar
    --   case c of
    --     '"' -> return $ Just ""
    --     _ -> Just <$> mconcat <$> many (esc <|> stringBytes)
    s = Just <$> mconcat <$> many (esc <|> stringBytes)
    esc = P.string "\\\""
    stringBytes = P.takeWhile1 (P.notInClass "\"\\")


-- Example $ let Right x = P.parseOnly  parseToJson "a=2 f emp=" in putStrLn x
parseToJson = encode <$> object <$> (fmap handlePair) <$> parseLine
  where
    handlePair (k, v) =
      case v of
        Nothing -> (k, Bool True)
        Just s -> (k, String s)

--	message = { garbage, pair }, garbage
--	pair = key, '=', value | key, '=' | key
--	value = ident | '"', { stringBytes | '\', '"' }, '"'
--	key = ident
--	ident = ident_byte, { ident byte }
--	garbage = !ident_byte
--	stringBytes = any byte excluding '"' and '\'
--	ident_byte = any byte greater than ' ', excluding '=' and '"'
