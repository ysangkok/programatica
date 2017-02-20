module board where
import Output(OUTPUT(..))
import utils(rightAdj,randlist)

--export startpos,board,outside,showscore,randpos,movebelow,

startpos = [(11,46),(11,12),(7,46),(7,12),
	    (15,46),(15,12),(3,46),(3,12),(19,46),(19,12)]
outside (y,x) = x<1 || x>55 || y<1 || y>20
showscore (n,score)=[Moveto 70 (2*n), Str' (rightAdj (show score) 5 ' ')]
board playernames =
	Clr: Str' border: Moveto 58 0: Str' "Score Board": concatMap showname playernames
	where showname (n,name)=[Moveto 58 (2*n), Str'(show n ++ " "), Flush,
				 Str' name, Flush]
	      border = hline ++ concat (replicate 20 ("*" ++ replicate 7 '\t' ++ "*\n")) ++ hline
	        where hline = replicate 57 '*' ++ "\n"

randpos = zip (randlist 123 1 20)(randlist 321 1 55)
--randpos = zip [5..] [5..]
movebelow = Moveto 0 22

{-end-}
