
module main where

import Data.List (isSuffixOf)
import System.Environment(getArgs)
import ProcessFile


main = do
   [file] <- getArgs
   let tup = %s 
   if isSuffixOf ".out" file  
      then processout file 
      else isSuffixOf ".xyz" file then processxyz tup file 
                                  else error "Unknown FilePath"


#! /bin/bash -l
#SBATCH -A snic2015-6-12
#SBATCH -p core 
#SBATCH -t 00:30:00 
#SBATCH -J AnalyzeDyn
#SBATCH -N 1 --cpus-per-task 2
