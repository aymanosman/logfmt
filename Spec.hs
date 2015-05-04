{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Attoparsec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.ByteString (ByteString)

import Logfmt

type B = ByteString

main = hspec $ do
  describe "Examples" $ do

    it "one" $ do
      ("key=value foo=\"a bar\"" :: B) ~> parseLine `shouldParse` [
        Pair "key" "value"
        , Pair "foo" "a bar"]

    it "two" $ do
      ("\"" :: B) ~> parseLine `shouldParse` []

    it "three" $ do
      ("a=foo b=10ms c=cat E=\"123\" d foo= emp=" :: B) ~> parseLine
        `shouldParse` [
        Pair "a" "foo"
        , Pair "b" "10ms"
        , Pair "c" "cat"
        , Pair "E" "123"
        , Pair "d" "true"
        , Pair "foo" ""
        , Pair "emp" ""]

-- s2 = "key=\"value\" foo=bar"
-- s3 = "key=\"a value\" foo=bar"
-- s4 = "a=1 b=2 c=3"
-- s5 = "schmoo=\"hello\nworld\""
-- s6 = "ip=127.0.0.1 time=2015-05-03T13:54:03.396Z method=GET path=\"/test/user/has-friend?friend=example\" status=304 content_length= content_type= elapsed=2ms"
-- s8 = "measure.a=1ms measure.b=10 measure.c=100MB measure.d=1s garbage"
-- s9 = "foo=bar a=14 baz=\"hello kitty\" cool%story=bro f %^asdf"

-- ss = [s6, s7, s8, s9]

-- main = do
--   forM_ ss (p parseLine)
--   where
--   p pr s = do
--     K.putStrLn s
--     let Right r = P.parseOnly pr s
--     mapM_ print r
--     putStrLn ""
