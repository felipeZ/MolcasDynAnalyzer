
module Main where

import Control.Monad
import Data.List (isSuffixOf,sort)
import System.Directory
import System.Environment (getArgs)
import System.FilePath.Posix ((</>))
import Text.Printf 
-- ===========> Internal Modules <============


import ProcessFile
import Types

-- =======> <============

main = do
  args <- getArgs
  if (length args) /= 3 then print msg1
                        else do
                            let [n,rootDir,tuple] = args
                                njobs = read n
                            xyzFiles <- getRecursiveContents "md.xyz" rootDir
                            outFiles <- getRecursiveContents "out" rootDir
                            processFiles njobs xyzFiles (launchLocalProcess tuple) handle
                            processFiles njobs outFiles (launchLocalProcess tuple) handle


-- mainCluster = do
--   args@[n,rootDir,tuple] <- getArgs
--   if (length args) /= 2 then print msg1
--                         else do
--                             let njobs = read n
--                             xyzFiles <- getRecursiveContents "xyz" rootDir
--                             outFiles <- getRecursiveContents "out" rootDir
--                             processFiles njobs xyzFiles (launchProcess tuple) handle
--                             processFiles njobs outFiles (launchProcess tuple) handle


  
  
msg1 :: String
msg1 = "the program required the number of the simulataneous jobs to be run in parallel, the path to the directory where the output files are store the atoms involved in the bond (a1,a2) together" 

handle :: FilePath -> IO ()
handle = printf "File %s could not being parse\n" 

getRecursiveContents :: String -> FilePath -> IO [FilePath]
getRecursiveContents suffix topdir = do
   names <- getDirectoryContents topdir
   let properNames = filter (`notElem` [".", ".."]) names
   paths <- forM properNames $ \name -> do
         let path = topdir </> name
         isDirectory <- doesDirectoryExist path
         if isDirectory 
               then getRecursiveContents suffix path 
               else return $ if isSuffixOf suffix name then [path] else []
   return . sort .  concat $ paths


readTInt :: String -> (Int,Int) 
readTInt x = read x :: (Int,Int)

