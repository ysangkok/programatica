module Arch (myStdGen, prCPU, fudlogueEdit, fudlogueWrite, fudlogueWriteUni, welcomeArch) where

import Time
import Random
import CPUTime
import Fudgets
import qualified EditSessionF as E --(fudlogueEdit)
import qualified UnicodeF as U --(fudlogueWrite)
import qualified State

-- architecture/compiler dependent definitions for unix/hbc

myStdGen :: Int -> IO StdGen ---
--- myStdGen _ = newStdGen --- gives always the same result
myStdGen int0 = do
  t0  <- getClockTime 
  cal <- toCalendarTime t0 
  let int = int0 + ctSec cal + fromInteger (div (ctPicosec cal) 10000000)
  return $ mkStdGen int

prCPU cpu = do cpu' <- getCPUTime
               putStr ("\n" ++ show ((cpu' - cpu) `div` 1000000000) ++ " msec")
               return cpu'

fudlogueEdit = E.fudlogueEdit
fudlogueWrite = U.fudlogueWrite
fudlogueWriteUni = fudlogueWrite State.myUniFont

welcomeArch = "This is the complete system compiled with hbc."
