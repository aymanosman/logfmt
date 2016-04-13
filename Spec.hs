{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Attoparsec
-- import Test.QuickCheck
import Data.Monoid
import Data.Text (Text, pack, unpack, isInfixOf)

import Logfmt

encode' :: [(Text, Maybe Text)] -> Text
encode' [] = ""
encode' ((a,b):xs) = a <> conv b <> " " <> encode' xs
  where
    conv x =
      case x of
        Just s -> "=" <> f s
        Nothing -> ""
    f s =
      if " " `isInfixOf` s
      then pack $ show s else s

main :: IO ()
main = hspec $
  describe "Examples" $ do

    let exp1 = [("key", Just "value") , ("foo", Just "a bar")]
    let s1 = encode' exp1
    it (unpack s1) $
      (s1 :: Text) ~> parseLine `shouldParse` exp1

    let s2 = "\""
    it (unpack s2) $
      (s2 :: Text) ~> parseLine `shouldParse` []

    let s3 = "a=foo b=10ms c=cat E=\"123\" d foo= emp="
    it (unpack s3) $
      (s3 :: Text) ~> parseLine `shouldParse`
      [ ("a", Just "foo")
      , ("b", Just "10ms")
      , ("c", Just "cat")
      , ("E", Just "123")
      , ("d", Nothing)
      , ("foo", Just "")
      , ("emp", Just "")
      ]

    let s4 = "a=2 b=\"a str\\\"ing\" f"
    it (unpack s4) $
      (s4  :: Text) ~> parseLine `shouldParse`
      [ ("a", Just "2")
      , ("b", Just "a str\\\"ing")
      , ("f", Nothing)
      ]

    let s5 = "f a=\" hello\\\"world\\\"\""
    it (unpack s5) $
      (s5 :: Text) ~> parseLine `shouldParse`
      [ ("f", Nothing)
      , ("a", Just " hello\\\"world\\\"")
      ]

    let s6 = "a=2 b=abc\"de f"
    it (unpack s6) $
      (s6::Text) ~> parseLine `shouldParse`
      [ ("a", Just "2")
      , ("b", Just "abc")
      ]

