{-# LANGUAGE DeriveDataTypeable, QuasiQuotes #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.List (isSuffixOf,sort)
import System.Console.CmdArgs
import System.Directory
import System.Environment (getArgs)
import System.FilePath.Posix ((</>))
import System.Process
import Text.Printf 
-- ===========> Internal Modules <============
import ProcessFile
import SlurmQuotes (slurmText)
import Types


-- ==========> Main <=============

data SystemInfo = SystemInfo {
                   njobs   :: Int
                  ,rootdir :: FilePath
                  ,bond    :: String
                  ,slurm   :: Bool
                   } deriving (Show, Data, Typeable)

sysinfo = SystemInfo
                  {njobs   = def &= help "Number jobs to run simultaneously"
                  ,rootdir = def &= help "Folder containing the trajectories" &= typDir
                  ,bond    = def &= help "tuple of the atoms involved in the bond"
                  ,slurm   = def &= help "if True run the calculation using the slurm system" &= typ "Bool"
                  } &=
                  verbosity &=
                  help "read excited states energies and the bond distance from the trajectories in the rootdir"


main = do
  sysData  <-  cmdArgs sysinfo
  let jobs   = njobs sysData
      dir    = rootdir sysData
      bs     = bond sysData
      remote = slurm sysData
  print remote
  outFiles <- getRecursiveContents ".out" dir
  if not remote
     then processFiles jobs outFiles (processout bs) handle
     else do let script = "prueba.sh"
             -- writeSlurm bs script "file"
             processFiles jobs outFiles (slurmProcess (show bs) ) handle

slurmProcess :: String -> FilePath -> IO ()
slurmProcess bs file = do
  let name = "parser.sh"
      xs  = scriptSlurm [file,bs]
  writeFile name xs 
  executeSlurm name

scriptSlurm :: [String] -> String
scriptSlurm [file,bs] =  script file bs

 where script = [slurmText|

#!/bin/bash
#SBATCH --time=00:15:00
#SBATCH --ntasks=2

RemoteParser %file %bond +RTS -N2

|]

executeSlurm :: FilePath -> IO ()
executeSlurm scriptName = do
    user <- (head . words) `fmap` readProcess "whoami" [] []
    pid  <- getpid `fmap` readProcess "sbatch" [scriptName] []
    loop pid user
    
  where loop i user  = do
          st <- getStatusPid i user
          if any (st==) ["R","PD","CG","CD"]          -- Process is running or pending 
             then threadDelay (10 * 10^6) >> loop i user -- sleep 10 seconds 
             else return ()
           

getpid :: String ->  String
getpid xs = undefined


getStatusPid :: String -> String -> IO  String
getStatusPid pid user= do
    st <- readProcess "squeue" ["-u",user,"-j",pid,"-o"," %.2t"] []
    if null st
       then return "" -- Process has finished
       else return . take 2 $ st 
  
-- main = do
--   args <- getArgs
--   if (length args) /= 3 then print msg1
--                         else do
--                             let [n,rootDir,tuple] = args
--                                 njobs = read n
--                             xyzFiles <- getRecursiveContents "md.xyz" rootDir
--                             outFiles <- getRecursiveContents "out" rootDir
--                             processFiles njobs xyzFiles (launchLocalProcess tuple) handle
--                             processFiles njobs outFiles (launchLocalProcess tuple) handle

  
  
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


