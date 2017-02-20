module play(play) where
import io
import Output
import move
import draw
import ask
import board
import Q
import utils
import Worm
import Keys
import ListMap(lookupWithDefault)

insideanyworm worms pos= any (insideworm pos) worms

-- show new piece of food when the old one has been eaten by a worm
newfood (_:rnd0,worms) =
  let (food:rnd)=dropto (not . insideanyworm worms) rnd0
  in pr (draw food '&') !!!
     return' (food:rnd,worms)

-- increase the score for a worm     show new score on the screen
score n (rnd,worms) =
  let newworms = anth n (updscore (+10) . updgrow (+10)) worms
  in pr (showscore (n,wormscore (newworms !! (n-1)))) !!!
     return' (rnd,newworms)

-- check if any worm has bumped into another worm
wormcrash _ []=[]
wormcrash ws2 (worm:ws)=
   let head=wormhead worm
   in if insideanyworm ws2 head || insideanyworm ws head
      then wormno worm:wormcrash (worm:ws2) ws
      else wormcrash (worm:ws2) ws

-- check if a worm has bumped into the food or another worm
checkbump selfcrash (state @ (food:_,worms)) =
  let eat=lindex [food] (map wormhead worms)+1
  in (if eat>0
      then score eat state !> newfood
      else return' state) !>
     (\(rnd,worms)->return' ((rnd,worms),selfcrash ++ wormcrash [] worms))
     --(\(rnd,worms)->return' ((rnd,worms),[] ))

-- move a worm forward     check if it crashed into the wall or itself
moveworm w =
  let head=wormhead w
  in let newhead=addv head (wormdir w)
         (drawtail,w2)= if wormgrow w > 0
                        then ([Str' ""],updgrow (+(-1)) w)
                        else afst erase (cutwormtail w)
  in let newworm= movewormhead w2 newhead
  --in let newworm= w2 
         selfcrash=if outside newhead || qmember (wormbody w2) newhead
                   then [wormno w]
                   else []
  in (drawtail ++ draw newhead (chr(wormno w `mod` 10 +ord '0')) ++ draw head 'o',newworm,selfcrash)
  --in let dir = [] -- moveto (0,0) ++ show_int (fst (wormdir w)) ++ "," ++ show_int (snd (wormdir w)) ++ " "
  --in (dir ++ drawtail ++ draw newhead (chr(wormno w `mod` 10 +ord '0')) ++ draw head 'o',newworm,selfcrash)
  --in (":",newworm,selfcrash)

moveworms [] = ([],[],[])
moveworms (worm:worms)=
   let (output,newworm,selfcrash)=moveworm worm
   --let (output,newworm,selfcrash)=(":",worm,[])
       (outputs,newworms,crashes)=moveworms worms
   in (output ++ outputs,newworm:newworms,selfcrash ++ crashes)
   --in (output ++ outputs,newworm:newworms,[])

forward (rnd,worms) =
  let (output,newworms,selfcrash)=moveworms (map changedir worms)
  --let (output,newworms,selfcrash)=moveworms worms
  --let (output,newworms,selfcrash)=(":",worms,[])
  in pr output !!! checkbump selfcrash (rnd,newworms)
  --in pr output !!! return' ((rnd,newworms),[])
  --in prs ":" !!! return' ((rnd,newworms),[])


dirs = [const up,const left,const right,const down,turnleft,turnright]
dirkey c=c `elem` stdkeys
keydir  =lookupWithDefault (zip stdkeys dirs) (error "play.keydir")

storedir n dirchange (rnd,worms) =
  (rnd,anth n (storedirchange dirchange) worms)

pause = pr [movebelow, Str' "Paused: Continue? "] !!!
            yesorno !>
            (\cont->if cont
                   then pr [movebelow, Str' "                 "]
                   else pause
            )

step state =
  pr [Flush] !!!
  getc (\k->case k
           of Tick-> forward state !>
                    (\(state,crash)->
                      if crash/=[]
                      then return' (state,crash)
                      else step state
                    )
              Key n c ->
		if dirkey c then step (storedir n (keydir c) state)
		else if c=='' then return' (state,[])
		else if c=='p' then pause !!! step state
		else step state
           )

showallscores (_,worms)=
  pr (concatMap (\w->showscore (wormno w,wormscore w)) worms)

play state= showallscores state !!!
	    newfood state !>
	    step
--and play state=step state

{-end-}
