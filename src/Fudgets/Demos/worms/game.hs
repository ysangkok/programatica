module game(masken,maxplayers) where
import io
import Output
import move
import draw
import board
import ask
import Q
import play
import Worm
import Keys(INPUT)
import utils(mix)
--import Trace

game players =
 let  startdir = left:right:startdir
      playernos = [1..players]
      scores0 = map (\n->(n,0)) playernos

      worms = map (\n->newworm n (startpos !! (n-1)) (startdir !! (n-1)) 9 0) playernos

      prnos nos = mix (map show nos) " "

      startgame rnd =
        countdown (take players startpos) 3 !!!
	return' (rnd,worms)

      gameover scores ((food:rnd,worms),crashed) =
	let newscores=map (\(n,s)->if elem n crashed
				  then (n,s)
				  else (n,s+1)) scores
	in pr [movebelow] !!!
	   (if crashed/=[]
	    then prs ("Worm no " ++ prnos crashed ++ " crashed!\n") !!!
		 (if players>1
		  then pr (concatMap showscore newscores)
		  else nop
		 ) !!!
		 pr [movebelow, Str' "\n"]
	    else prs "Aborted:\n") !!!
	   prs "Game over! Play again? " !!!
	   yesorno !>
	   (\again->
	     (if again
	      then pr (Str' "y":Flush:erase food ++
		       concatMap (erasemask . wormbody) worms ++
		       [movebelow, Str' "                     \n",
		       Str' "                        "])
	      else prs "n\nBye bye!\n") !!!
	     return' ((newscores,rnd),again)
	   )
 in repuntil (\(scores,rnd)->
		startgame rnd !>
		play !>
		gameover scores) (scores0,randpos)

--in 
masken players playernames = program (pr (board playernames) !!! game players)
  --masken players = program ( pr Clr !!! game players )

maxplayers = length startpos
