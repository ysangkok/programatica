module Worm(
	WORM,
	wormno,wormhead,wormbody,wormdir,wormgrow,	-- selectors
	wormscore,
	updhead,updbody,upddir,updgrow,updscore,	-- update functions
	newworm,					-- constructor
	insideworm,					-- predicates
	cutwormtail,movewormhead,			-- special modifiers
	changedir,storedirchange
 ) where
import Q
import utils
import move
import Output(OUTPUT)

data WORM = Worm
      {	wormno   :: Int,		-- worm number
    	wormhead :: Pos,		-- position of head
	wormbody :: (QUEUE Pos),	-- worm body (including head)
	wormdir  :: Dir,		-- current direction
	wormgrow :: Int,		-- how much to grow
	wormscore :: Int,		-- score
	wormdirq :: (QUEUE ((Int,Int)->(Int,Int)))
      }					-- outstanding direction changes

type Pos = (Int,Int)
type Dir = (Int,Int)

updhead f w@(Worm { wormhead=wormhead }) = w { wormhead = f wormhead }
updbody f w@(Worm { wormbody=wormbody }) = w { wormbody = f wormbody }
upddir f w@(Worm { wormdir=wormdir }) = w { wormdir = f wormdir }
updgrow f w@(Worm { wormgrow=wormgrow }) = w { wormgrow = f wormgrow }
updscore f w@(Worm { wormscore=wormscore }) = w { wormscore = f wormscore }
upddirq f w@(Worm { wormdirq=wormdirq }) = w { wormdirq = f wormdirq }

newworm no head dir grow score=
    Worm no head (Enter Empty head) dir grow score Empty

insideworm pos worm= qmember (wormbody worm) pos

cutwormtail w@(Worm {wormbody=body})=
    let (tail,rest)=qremove body
    in (tail,w {wormbody=rest})

movewormhead (Worm n _ body d g s q) newhead=
    let newbody = (Enter body newhead)
    in mwh (Worm n newhead) d g s q newbody 
mwh w d g s q n=w n d g s q
{-
      movewormhead newhead (Worm n head body d g s q)=
    Worm n newhead (Enter body newhead) d g s q
-}

changedir worm@(Worm {wormdir=dir,wormdirq=dirchangeq})=
    if isempty dirchangeq
    then worm
    else let (dirchange,rest)=qremove dirchangeq
	     newdir=dirchange dir
	     newdir'=if newdir==negv dir
		     then dir
		     else newdir
         in worm { wormdir=newdir' , wormdirq= rest }

storedirchange dirchange w@(Worm {wormdirq=dirchangeq})=
    w {wormdirq = Enter dirchangeq dirchange}
