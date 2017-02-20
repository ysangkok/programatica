module	Main(main) where -- A very simple adding machine
import Fudgets

main = fudlogue (shellF "Adder" adderF)

adderF = placerF (revP verticalP) $
         ("Sum" `labLeftOfF` intDispF) >==<
	 mapstateF add 0 >==<
	 ("Input" `labLeftOfF` intInputF)
  where
    add acc n = (acc+n,[acc+n])
