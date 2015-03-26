{-# Language DeriveDataTypeable #-}

module Types where

import Control.Exception 
import Data.Typeable

-- =============> <====================
type Vec3D = [Double]

type MolCoord = [Vec3D]

data Action = XYZ String | OUT deriving show

data MolcasException = XYZException | OutException deriving (Show,Typeable)

instance Exception MolcasException


