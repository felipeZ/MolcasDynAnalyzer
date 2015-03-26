
module Molcas where

import Control.Applicative ((<$>),(<*>),(*>),(<*))
import Control.Exception (throwIO)
import Data.List (transpose)
import Text.Parsec
import Text.Parsec.ByteString

-- ======> Internal imports <=========
import Types 
import ParsecText

-- ===========> <==========================
parseMolcasOutput :: FilePath -> IO [[Double]]
parseMolcasOutput fileName = do 
 r <- parseFromFile (many1 $ try $ molcasEnergies) fileName 
 case r of
      Left  msg  -> error $ show msg -- throwIO OutException
      Right energies -> return $ transpose energies


molcasEnergies :: Parser [Double]
molcasEnergies = do 
    manyTill anyChar $ try $ string "      Final state energy(ies):"
    newline
    string "      ------------------------"
    newline
    anyLine
    count 4 rootEnergy


-- Parse rasscf root Energy
-- ::    RASSCF root number  2 Total energy =       -821.43755900                                                          
rootEnergy :: Parser Double
rootEnergy = try $ do
    manyTill anyChar $ try $ string "::    RASSCF root number"
    manyTill anyChar $ try $ string "Total energy:"
    spaces
    energy <- readD <$> spaceFloat 
    anyLine
    return energy

