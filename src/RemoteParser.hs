
module RemoteParser where

import System.Environment (getArgs)

-- ===========> Internal Modules <============
import ProcessFile

-- ==========> Main <=============

main = do
 [file,bs] <- getArgs -- MolcasOutput and bond
 processout bs file 
 
 

