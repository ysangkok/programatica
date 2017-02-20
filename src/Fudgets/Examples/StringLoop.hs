import Fudgets

main = fudlogue (shellF "String Loop" $ tstF)

tstF = dispF>==<loopF(strF>==<strF>==<strF)

strF = "In" `labLeftOfF` (stripInputSP>^^=<stringF)

dispF = "Out" `labLeftOfF` displayF
