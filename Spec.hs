{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Attoparsec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Text (Text, unpack)
import Data.Aeson

import Logfmt

type T = Text

main = hspec $ do
  describe "Examples" $ do
    let s = "key=value foo=\"a bar\""
    it (unpack s) $ do
      (s :: T) ~> parseLine `shouldParse` [
        ("key", Just "value")
        , ("foo", Just "a bar")
        ]

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

    it (unpack s) $ do
      (s :: T) ~> parseLine `shouldParse` [
        ("emp", "\\")
        ]

