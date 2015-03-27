
module ProcessFile where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception 
import Control.Monad
import Control.Parallel.Strategies (Strategy,parList,parMap,rdeepseq,rseq,using)
import Data.Char (isSpace)
import Data.List (dropWhile)
import System.FilePath.Posix (splitFileName)
import System.Process (readProcess)
import Text.Printf 

-- ==================> Internal Modules <======================

import Molcas 
import NumericalFunctions 
import ParserXYZ
import Types

-- =====================><=========================

type PID = String  

-- -------------------------> <--------------------

-- |executes a list of IO action in such a way that  N actions are run concurrentely. 
-- |This function spawns an async process that execute an IO action.
processFiles ::  Int ->   -- | number of concurrent jobs 
                 [FilePath] -> -- | Files to process
                 (FilePath -> IO ()) -> -- | action to carry out with the file
                 (FilePath -> IO ()) -> -- | action in case of failure
                 IO ()
processFiles n fs  action handle = do
                 tbQ  <- atomically $ newTBQueue n
                 loop fs tbQ
 where loop :: [FilePath] -> TBQueue FilePath -> IO () 
       loop xs queue | null xs = return ()  
                     | otherwise  = do 
                              join . atomically $ do 
                                writeTBQueue queue (head xs)
                                let actionSTM = atomically $ readTBQueue queue
                                return $ withAsync actionSTM $ \a -> do 
                                           file <- wait a 
                                           b    <- async $ action file
                                           doSomethingOnException file b
                              loop (tail xs) queue
       doSomethingOnException  :: FilePath -> Async () -> IO ()
       doSomethingOnException  file a = do 
           r <- waitCatch a
           case r of
                Left e    -> handle file 
                Right _   -> return ()

-- | Run the script that launch the parser on the cluster and wait for it
launchProcess :: String -> FilePath -> IO ()
launchProcess arg file = do
         writeScript  "runParser.sh" arg   
         pid <- submitScript "sbatch" ["runParser.sh"]
         waitCluster pid   

writeScript :: FilePath  -> String -> IO ()
writeScript file arg =  writeFile "runParser.sh" content
 
 where content = printf "%ssbatch clusterParser %s %s\n" header file arg
       header = "#! /bin/bash -l\n#SBATCH -A snic2015-6-12\n#SBATCH -p core \n#SBATCH -t 00:30:00 \n#SBATCH -J AnalyzeDyn\n#SBATCH -N 1 --cpus-per-task 2\n\n"


submitScript :: FilePath -> [String] -> IO PID
submitScript file opts =  (readProcess file opts []) >>= return . getPID 
 where getPID = read . last . words

waitCluster :: PID -> IO ()
waitCluster pid = do 
    st <- fmap getStatus $ readProcess "squeue" ["j",pid,"--format=\"%.2t\""] [] 
    delay
    if st `elem` ["R","PD"] then delay >> waitCluster pid 
                            else return ()    
  
 where getStatus = dropWhile isSpace . last . lines . filter ( /= '\"')
       delay = threadDelay (30 * 1000000) 
                            
-- | Calculate the bond distance using the supply atom numbers and the 
-- | cartesian coordinates
processxyz :: FilePath -> (Int,Int) -> IO ()
processxyz path (a1,a2) = do 
               xyz <- parseXYZFile path 
               let bonds    = parMap rseq (calculateBond a1 a2) xyz
                   rs       = concatMap (printf "%10.5f\n") bonds
                   (dir,_)  =  splitFileName path
                   fileOut  = printf "%sbonds.out" dir 
               writeFile fileOut rs

processout :: FilePath -> IO ()
processout  path = do 
   out <- parseMolcasOutput path
   let (dir,_) =  splitFileName path
       ess     = map (concatMap (printf "%10.5f\n")) out
       files   = map (printf "%senergies%.out" path) ["S0","S1","S2","S3"]
   zipWithM_ writeFile files ess 


