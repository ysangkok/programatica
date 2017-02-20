import Fudgets

main = fudlogue (shellF "Pocket Calculator" calcF)

calcF = intDispF >==< mapstateF calc [0] >==< buttonsF

data Buttons = Plus | Minus | Times | Div | Enter | Digit Int   deriving (Eq)

buttonsF = placerF (matrixP 4) (
              listF [d 7, d 8, d 9,op Div,
                     d 4, d 5, d 6,op Times,
		     d 1, d 2, d 3,op Minus,
		     hole,d 0,ent, op Plus])
  where
    d n = (Digit n,buttonF (show n))
    ent = op Enter
    hole = (Enter,holeF)
    op o = (o,buttonF (opLabel o))
      where opLabel Plus = "+"
            opLabel Minus = "-"
	    opLabel Times  = "*"
	    opLabel Div = "/"
	    opLabel Enter = "Ent"

calc (n:s)   (Digit d,_) = new (n*10+d) s
calc s       (Enter,_)   = (0:s,[])
calc (y:x:s) (Plus,_)    = new (x+y) s
calc (y:x:s) (Minus,_)   = new (x-y) s
calc (y:x:s) (Times,_)   = new (x*y) s
calc (y:x:s) (Div,_)     = new (x `div` y) s
calc s       _           = (s,[])

new n s = (n:s,[n])
