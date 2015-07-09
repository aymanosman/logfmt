{-# LANGUAGE OverloadedStrings #-}
module Logfmt.Json where

import qualified Logfmt as L
import Data.Aeson

-- Example $ let Right x = P.parseOnly  parseToJson "a=2 f emp=" in putStrLn x
parseToJson = encode <$> object <$> (fmap handlePair) <$> L.parseLine
  where
    handlePair (k, v) =
      case v of
        Nothing -> (k, Bool True)
        Just s -> (k, String s)
