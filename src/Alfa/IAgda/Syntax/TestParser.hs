module Main where
import System(getArgs)
import ParAgda(pModule)
import MyTestAgda

main = do
         [f] <- getArgs
         runFile pModule f
