{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Attoparsec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Monoid
import Data.Text (Text, pack, unpack, isInfixOf)

import Logfmt

type T = Text

encode' [] = ""
encode' ((a,b):xs) = a <> conv b <> " " <> encode' xs
  where
    conv b = case b of
      Just s -> "=" <> f s
      Nothing -> ""
    f s = if " " `isInfixOf` s
          then pack $ show s else s

main = hspec $ do
  describe "Examples" $ do

    let exp1 = [("key", Just "value") , ("foo", Just "a bar")]
    let s = encode' exp1
    it (unpack s) $ do
      (s :: T) ~> parseLine `shouldParse` exp1

    let s = "\""
    it (unpack s) $ do
      (s :: T) ~> parseLine `shouldParse` []

    let s = "a=foo b=10ms c=cat E=\"123\" d foo= emp="
    it (unpack s) $ do
      (s :: T) ~> parseLine
        `shouldParse` [
         ("a", Just "foo")
        , ("b", Just "10ms")
        , ("c", Just "cat")
        , ("E", Just "123")
        , ("d", Nothing)
        , ("foo", Just "")
        , ("emp", Just "")
        ]

    let s = "a=2 b=\"a str\\\"ing\" f"
    it (unpack s) $ do
      (s  :: T) ~> parseLine `shouldParse` [
        ("a", Just "2")
        , ("b", Just "a str\\\"ing")
        , ("f", Nothing)
        ]

    let s = "f a=\" hello\\\"world\\\"\""
    it (unpack s) $ do
      (s :: T) ~> parseLine `shouldParse` [
        ("f", Nothing)
        , ("a", Just " hello\\\"world\\\"")
        ]

    let s = "a=2 b=abc\"de f"
    it (unpack s) $ do
      (s::T) ~> parseLine `shouldParse` [
        ("a", Just "2")
        , ("b", Just "abc")
        ]

