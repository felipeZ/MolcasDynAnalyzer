
module Main where

import Data.List (isSuffixOf)
import System.Environment(getArgs)
import ProcessFile (processout,processxyz)


main = do
   [file,tup] <- getArgs
   let bond = readT tup   
   if isSuffixOf ".out" file  
      then processout file 
      else if isSuffixOf ".xyz" file 
              then processxyz file bond 
              else error "Unknown FilePath"


readT :: String -> (Int,Int)
readT = read 
