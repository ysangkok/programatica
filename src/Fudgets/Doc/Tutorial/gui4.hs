import Fudgets

main = fudlogue (shellF "Fac" facF)

facF = placerF (revP verticalP) (
        ("x! =" `labLeftOfF` intDispF) >==<
        mapF fac >==<
	("x =" `labLeftOfF` intInputF))

fac 0 = 1
fac n = n * fac (n-1)
