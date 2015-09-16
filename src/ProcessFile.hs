
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

import NumericalFunctions 
import ParseMolcas
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

-- | Calculates the bond distance using the supply atom numbers and the 
--  cartesian coordinates. Then prints those values together with the
--  energies of the first 4 roots  
processout :: String -> FilePath -> IO ()
processout t path = do 
   (xss,energies) <- parseMolcasOutput path
   let (a1,a2)   = readT t
       bonds     = parMap rseq (calculateBond a1 a2) xss
       rs        = concatMap (printf "%10.5f\n") bonds
       (dir,_)   =  splitFileName path
       ess       = map (concatMap (printf "%10.5f\n")) energies
       files     = map (printf "%senergies%s.txt" dir) ["S0","S1","S2","S3"]
       fileBonds = printf "%sbonds.txt" dir
   writeFile fileBonds rs       
   zipWithM_ writeFile files ess 


readT :: String -> (Int,Int)
readT = read
