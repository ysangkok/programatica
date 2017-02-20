module Panel  where -- (panel,defaultcellsize) where

import Fudgets
import Timer

quitShell title f = shellF title 
	  (stripEither >^=< (f >+< quitButtonF) >=^< Left)

panel = quitShell "Panel" (vBoxF $ runButton >+< sizegroup)

runButton = timer interval >==< toggleButtonF "Run" 

interval = 200 -- milliseconds

sizegroup = radioGroupF sizes defaultcellsize

sizes = [(3::Int,"Tiny"), (5,"Small"),(10,"Medium"),(25,"Huge")]

defaultcellsize = fst (sizes !! 2)
