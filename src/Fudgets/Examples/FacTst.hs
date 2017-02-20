module Main where
import Fudgets

main = fudlogue (shellF "FacTst" mainF)

mainF = outFacF >==< inIntF

inIntF = "x=" `labLeftOfF` (inputDoneSP>^^=<intF)

outFacF = "fac(x)=" `labLeftOfF` intDispF >=^< fac

fac 0 = 1
fac n = n * fac(n-1)
