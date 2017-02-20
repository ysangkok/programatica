module Arch (myStdGen, prCPU, fudlogueEdit, fudlogueWrite, fudlogueWriteUni, welcomeArch) where

import Operations
import Random

-- architecture/compiler dependent definitions for unix/hugs

myStdGen :: Int -> IO StdGen ---
myStdGen int0 = newStdGen

prCPU cpu = return cpu


fudlogueEdit _ _ resume = do
  putStrLn "sorry no fudgets available in Hugs"
  resume

fudlogueWrite _ _ _ = do
  putStrLn "sorry no fudgets available in Hugs"

fudlogueWriteUni _ _ = do
  putStrLn "sorry no fudgets available in Hugs"

welcomeArch = 
  "This is the hugs version without fudgets, unicode, cpu timing," ++++
  "and good random generation. It is slower than the compiled version," ++++
  "as well."

