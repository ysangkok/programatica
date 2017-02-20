module Main where
import Fudgets

-- Cat - copies characers from standard input to standard output.

--main = fudlogue (stdoutF>==<stdinF)

-- Enhancement to quit on end-of-file:
main = fudlogue (stdoutF>==<quitIdF (=="")>==<stdinF)
