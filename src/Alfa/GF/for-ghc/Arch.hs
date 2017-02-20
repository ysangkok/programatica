module Arch (myStdGen, prCPU, fudlogueEdit, fudlogueWrite, fudlogueWriteUni, welcomeArch) where

import Time
import Random
import CPUTime

-- architecture/compiler dependent definitions for unix/ghc

myStdGen :: Int -> IO StdGen ---
myStdGen int0 = do
  t0  <- getClockTime 
  cal <- toCalendarTime t0 
  let int = int0 + ctSec cal + fromInteger (div (ctPicosec cal) 10000000)
  return $ mkStdGen int

prCPU cpu = do cpu' <- getCPUTime
               putStr ("\n" ++ show ((cpu' - cpu) `div` 1000000000) ++ " msec")
               return cpu'

fudlogueEdit _ _ resume = do
  putStrLn "sorry no fudgets available in ghc"
  resume

fudlogueWrite _ _ _ = do
  putStrLn "sorry no fudgets available in ghc"

fudlogueWriteUni = fudlogueWrite "" --- myUniFont

welcomeArch = 
  "This is the system compiled with ghc - fast but without fudgets."

