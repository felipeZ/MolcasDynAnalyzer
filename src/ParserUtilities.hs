
module ParserUtilities where

import Data.Attoparsec.ByteString.Char8


-- =============================> <==========================
anyLine :: Parser String
anyLine = takeTill  (== '\n') -- whatever chars we find till we hit a newline

