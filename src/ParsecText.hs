
module ParsecText where

import Control.Applicative ((<$>),(<*>),(*>),(<*))
import Text.Parsec 
import Text.Parsec.ByteString  (Parser)


-- =============================> <==========================
anyLine :: Parser String
anyLine = manyTill anyChar newline     -- whatever chars we find till we hit a newline

spaceAlphaNum :: Parser String
spaceAlphaNum = spaces *> many1 alphaNum

spaceFloat :: Parser String
spaceFloat = spaces *> many1 (char '-' <|> alphaNum <|> char '.')

manyDigit :: Parser String
manyDigit = spaces *> many digit 

readD :: String -> Double
readD x = read x :: Double
