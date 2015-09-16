
module SlurmQuotes (
                   slurmText
                   ) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

-- Internal Modules
import SlurmParse (parseSlurmExp)


slurmText :: QuasiQuoter
slurmText = QuasiQuoter 
  { quoteExp  = slurmQuotes
  , quotePat  = undefined
  , quoteDec  = undefined
  , quoteType = undefined
  }

slurmQuotes s = do
  pos <- getPosition
  parseSlurmExp pos s
  -- exp <- parseSlurmExp pos s
  -- dataToExpQ (const Nothing) exp

  
getPosition = fmap transPos TH.location
  where transPos loc = (TH.loc_filename loc,
                  fst (TH.loc_start loc),
                  snd (TH.loc_start loc))
