module	Main(main) where -- A very simple adding machine
import Fudgets

main = fudlogue (shellF "Adder" adderF)

adderF = intDispF >==< mapstateF add 0 >==< intInputF
  where
    add acc n = (acc+n,[acc+n])
