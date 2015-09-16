{-# LANGUAGE OverloadedStrings #-}

module ParseMolcas where 


import Control.Applicative ((*>),(<*),(<$>),(<*>))
import Control.Exception (throwIO)
import Data.Attoparsec.ByteString.Char8 as C
import qualified Data.ByteString.Char8  as B
import Data.List (transpose)
-- ==============> Internal Modules <===================

import Types 

-- ===============> <=======================

parseMolcasOutput :: FilePath -> IO ([MolCoord],[Energies])
parseMolcasOutput fileName = do
 logFile <- B.readFile fileName
 case parseOnly parseMolcas logFile of
      Left  msg        -> throwIO OutException 
      Right (mols,ess) -> return (mols, transpose ess)

parseMolcas :: Parser ([MolCoord],[Energies])
parseMolcas =  do
  n         <- countAtoms 
  unzip <$> (many1 $ parseData n)

prueba :: Parser ([MolCoord],[Energies])
prueba =  do
  n         <- countAtoms 
  unzip <$> (count 5  $ parseData n)

parseData :: Int -> Parser (MolCoord,Energies)
parseData n = do
    xyz <- parseCoordinates n
    es  <- parseEnergies
    return (xyz,es)

countAtoms :: Parser Int
countAtoms = 
  do  manyTill anyChar (string "Center  Label")  *> anyLine'
      xs <- B.unpack <$> takeTill (== '*')
      return $ length $ filter (isAlpha_ascii . head ) $ words xs

parseCoordinates :: Int -> Parser MolCoord
parseCoordinates n = manyTill anyChar till *> count 4 anyLine'  *> parserXYZ n
  where till = string "Old Coordinates"
                   
parserXYZ :: Int -> Parser MolCoord
parserXYZ n = count n $
          spaceDecimal *> spaceAscii *> count 3 spaceDouble <* anyLine'


parseEnergies :: Parser [Double]
parseEnergies = manyTill anyChar (string "Final state energy(ies):") *>
                count 3 anyLine' *>
                count 4 rootEnergy

rootEnergy :: Parser Double
rootEnergy = skipWhile (/= '-') *> double

-- ---------------------------------------------------


anyLine :: Parser B.ByteString
anyLine = takeTill  (== '\n') -- whatever chars we find till we hit a newline

anyLine' :: Parser ()
anyLine' = anyLine *> endOfLine  -- whatever chars we find till we hit a newline


spaceDecimal :: Parser Int
spaceDecimal = takeWhile1 isSpace *> decimal

spaceDouble :: Parser Double
spaceDouble = takeWhile1 isSpace *> double   

spaceAscii :: Parser B.ByteString
spaceAscii =  takeWhile1 isSpace *> takeWhile1 isAlpha_ascii
