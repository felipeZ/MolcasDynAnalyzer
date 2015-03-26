
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

types PID = String  

-- -------------------------> <--------------------

-- |executes a list of IO action in such a way that  N actions are run concurrentely. 
-- |This function spawns an async process that execute an IO action.
processFiles ::  Int -> [FilePath] -> (FilePath -> IO ()) -> IO ()
processFiles n fs  handle = do
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
                                           b    <- async $ launchProces file
                                           doSomethingOnException file b
                              loop (tail xs) queue
       doSomethingOnException  :: FilePath -> Async () -> IO ()
       doSomethingOnException  file a = do 
           r <- waitCatch a
           case r of
                Left e    -> handle file 
                Right _   -> return ()

-- | Writes a new Main.hs containing the desired computations, that is copy to the cluster node
-- | together with all the modules that main depends on and then it is executed remotely 
-- | with "runhaskell Main.hs [opts]". Then the async process should ask periodically to 
-- | the job scheduling system (using threadDelay) if the job is done.
launchProcess :: FilePath -> IO ()
launchProcess file action = do
         distributesCode
         writeScript  "runParser.sh"   
         pid <- submitScript "sbatch" ["runParser.sh"]
         waitCluster pid   

writeScript :: FilePath  -> IO FilePath
writeScript file action =  writeFile "runParser.sh" content
 
 Where content = printf "%ssbatch ./Main %s\n" header file
       header = "#! /bin/bash -l\n#SBATCH -A snic2015-6-12\n#SBATCH -p core \n#SBATCH -t 00:30:00 \n#SBATCH -J AnalyzeDyn\n#SBATCH -N 1 --cpus-per-task 2\n\n"


submitScript :: FilePath -> [String] -> IO PID
submitScript file opts =  readProcess file opts [] >>= return $ getPID 
 where getPID = read . last . words

waitCluster :: PID -> IO ()
waitCluster pid = do 
    st <- fmap getStatus $ readProcess "squeue" ["j",pid,,"--format=\"%.2t\""] [] 
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


clusterHeader :: String 
clusterHeader = undefined
