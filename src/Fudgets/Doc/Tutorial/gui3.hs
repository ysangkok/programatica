import Fudgets

main = fudlogue (shellF "Fac" facF)

facF = intDispF >==< mapF fac >==< intInputF

fac 0 = 1
fac n = n * fac (n-1)


