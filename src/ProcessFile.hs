
module ProcessFile where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception 
import Control.Monad
import Control.Parallel.Strategies (Strategy,parList,parMap,rdeepseq,rseq,using)
import Data.Char (isSpace)
import Data.List (dropWhile,isSuffixOf)
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
processFiles ::  Int -> [FilePath] -> (FilePath -> IO ()) -> (FilePath -> IO ()) -> IO ()
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

-- | Writes a new Main.hs containing the desired computations, that is copy to the cluster node
-- | together with all the modules that main depends on and then it is executed remotely 
-- | with "runhaskell Main.hs [opts]". Then the async process should ask periodically to 
-- | the job scheduling system (using threadDelay) if the job is done.
launchLocalProcess :: String -> FilePath -> IO ()
launchLocalProcess arg file = do
   let bond = readT arg
   if isSuffixOf ".out" file  
      then processout file 
      else if isSuffixOf ".xyz" file 
              then processxyz file bond 
              else error "Unknown FilePath"

readT :: String -> (Int,Int)
readT = read 
                            
-- | Calculate the bond distance using the supply atom numbers and the 
-- | cartesian coordinates
processxyz :: FilePath -> (Int,Int) -> IO ()
processxyz path (a1,a2) = do 
               xyz <- parseXYZFile path 
               let bonds    = parMap rseq (calculateBond a1 a2) xyz
                   rs       = concatMap (printf "%10.5f\n") bonds
                   (dir,_)  =  splitFileName path
                   fileOut  = printf "%sbonds.txt" dir 
               writeFile fileOut rs

processout :: FilePath -> IO ()
processout  path = do 
   out <- parseMolcasOutput path
   let (dir,_) =  splitFileName path
       ess     = map (concatMap (printf "%10.5f\n")) out
       files   = map (printf "%senergies%s.txt" dir) ["S0","S1","S2","S3"]
   zipWithM_ writeFile files ess 


