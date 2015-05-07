
module ParserXYZ where 


import Control.Applicative ((*>),(<*),(<$>),(<*>))
import Control.Exception (throwIO)
import Text.Parsec 
import Text.Parsec.ByteString  (Parser,parseFromFile)

-- ==============> Internal Modules <===================

import ParsecText
import Types 

-- ===============> <=======================

parseXYZFile :: FilePath -> IO [MolCoord]
parseXYZFile fileName = do 
 r <- parseFromFile parserXYZ fileName 
 case r of
      Left  msg  -> throwIO XYZException 
      Right mols -> return mols

parserXYZ :: Parser [MolCoord]
parserXYZ = many1 $ try parserMol

parserMol :: Parser MolCoord 
parserMol = (read <$> (spaceAlphaNum <* anyLine)) >>=  (flip count parseAtomCoord) 

parseAtomCoord :: Parser Vec3D
parseAtomCoord = spaceAlphaNum *> ((map readD) <$> (count 3 spaceFloat))

