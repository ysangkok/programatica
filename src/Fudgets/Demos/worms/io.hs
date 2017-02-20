module io(SP,SPm(..),Mk(..),Cont(..),
	(!!!),	-- Sequence two commands (like ; in Pascal)
	(!>),	-- Like !!!, but the first commands sends value to the second 
	pr,	-- print command
	program,-- Connect a command to an input stream
	nop,	-- Left operation
	--get,	-- General command to read from input stream
	getc,	-- Get a character from input stream
	--getline,-- Get line from input stream
	repuntil, -- repeat a command until it returns True
	repeat',	-- repeat' a command a given number of times
	return'	-- Return a value to command after !>
 ) where
import AllFudgets {- SPmonad -}

infixl !!!
infixl !>

-- Some imperative like IO support: Now based on stream processors.


pr s = putsSPm s
(!!!) = (>>)
program = monadSP
nop = nullSPm
getc = (>>=) getSPm
--and get parse p (ins,outs)=(p item (rest,outs) where (item,rest)=parse ins)
--and getline = get (splitat '\n')

return' = return
fetch p = p

(!>)= (>>=)

repeat' 0 cmd = nop
repeat' n cmd = cmd !!! repeat' (n-1) cmd

repuntil cmd state=
	cmd state !>
	(\(state,again)->if again then repuntil cmd state else nop)
