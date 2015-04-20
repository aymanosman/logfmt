module Logfmt where

import Control.Applicative
import Data.Attoparsec.ByteString as P
import Data.Attoparsec.ByteString.Char8 as C
import qualified Data.ByteString.Char8 as K

p1 = P.takeWhile (P.notInClass "=\"")
p2 = p1 <* char8 '='

-- main = P.parse p (K.pack "key=\"value\"")

test = do
  t p1 s (Right "key")
  t p2 s (Right "=")
  where
    s = "key=\"value\""
    t p s res = case P.parse p (K.pack s) of
      res -> putStrLn "Success :)"
      _ -> putStrLn "Fail :("
