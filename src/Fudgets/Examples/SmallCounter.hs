module	Main(main) where -- A very simple counter
import Fudgets

main = fudlogue (shellF "Counter" counterF)

counterF = hBoxF $ intDispF >==< absF countSP >==< buttonF "Increment"

countSP = mapAccumlSP inc 0
  where inc n _ = (n+1,n+1)
