module Main where -- The "Hello, World!" program
import Fudgets

main = fudlogue (shellF "Hello" helloF)

helloF = quitF >==< buttonF "Hello, World!"
