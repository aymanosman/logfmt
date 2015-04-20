module Logfmt where

import Data.Attoparsec.ByteString as P
import Data.ByteString.Char8 as C

p = P.takeWhile (P.notInClass "=\"")

main = parse p (C.pack "key=\"value\"")
