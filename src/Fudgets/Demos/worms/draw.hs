module draw where
import move
import Q
import board	-- outside
import Output(OUTPUT(..))
--export draw,erase,erasemask,


draw pos c = moveto pos ++ [Str' [c]]
erase pos = draw pos (if outside pos then '*' else ' ')
erasemask mask=
	if isempty mask
	then []
	else let (tail,rest)=qremove mask
	     in erase tail ++ erasemask rest

{-end-}
