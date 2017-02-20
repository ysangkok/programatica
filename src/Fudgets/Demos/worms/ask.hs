module ask where
import io
import Output
import utils
import move
import Keys

--export yesorno,countdown,

yesorno =
      pr [Flush] !!!
      getc (\c->case c
	       of Key _ 'n'-> return' False
	          Key _ 'y'-> return' True
	          _	   ->  yesorno
	       {-end-}
	   )

gettick = getc (\c->case c of Tick-> nop
                              _ -> gettick {-end-})

countdown pos =
  let     cnt 0 = nop
          cnt n = pr (concatMap (\p->moveto p ++ [Str'(show n)]) pos ++ [Flush]) !!!
		  repeat' 5 gettick !!! cnt (n-1)	-- 5*200ms=1s !!
  in cnt

{-end-}
