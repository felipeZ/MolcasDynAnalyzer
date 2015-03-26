

module Main where

import Data.List (isSuffixOf)
import System.Environment(getArgs)
import ProcessFile (processout,processxyz)


main = do
   [file,tup] <- getArgs
   let bond = readT tup   
   if isSuffixOf ".out" file  
      then processout file 
      else isSuffixOf ".xyz" file then processxyz bond file 
                                  else error "Unknown FilePath"
