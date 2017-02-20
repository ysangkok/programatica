module Main(main) where
import Fudgets

-- This program shows a string input field and a string display.
-- Whatever you enter in the string input field is copied to stdout.
-- Whatever is input on stdin is displayed in the string display.

main = fudlogue (shellF "Talk" (inF>+<outF))

outF = stdoutF >=^< (++"\n") >==< labLeftOfF "Out" strF
inF = labLeftOfF "In" dispF >==< (inputLinesSP>^^=<stdinF)

strF = inputDoneSP >^^=< startupF ["Hej"] stringF
dispF = displayF
