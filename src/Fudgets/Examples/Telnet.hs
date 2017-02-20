-- Telnet - a simple version of the telnet program (line buffered, local echo).
import Fudgets
import IO
import Ix

chr = toEnum

main = case args of
	 [host] -> fudlogue (telnetF host)
	 _      -> hPutStr stderr "Usage: Telnet <host>\n"

telnetF host =
  loopF (stdioF>==<
         optHanF (quitIdF (=="")>==<openSocketF host service transceiverF))

service = argReadKey "port" 23 -- 23 is the telnet service

--optHanF = id -- works for services other than telnet, e.g. smtp, nntp, ftp...

optHanF f = loopLeftF (optionsF>==<f>=^<stripEither)


-- Options negotiation is explained in Internet RFC 854,
-- URL http://cwis.auc.dk/rfc/rfc/rfc854.html

optionsF = options ""
  where
    iac=chr 255
    will=chr 251; wont=chr 252; do'=chr 253; don't=chr 254
    neg=(will,don't)
    names=["Will","Won't","Do","Don't"]
    putR s = putF (Left s) -- output to remote
    putL s = putF (Right s) -- output to local
    options s =
        case s of
          "" -> getF $ options
	  '\xff':r ->
	     case r of
	       "" -> getMore
	       cmd:r ->
	         if inRange neg cmd then
		   case r of
		     "" -> getMore
		     opt:r ->
		       case cmd of
		         '\xfd' -> putR [iac,wont,opt] $ options r
			 '\xfb' -> putR [iac,don't,opt] $ options r
			 _      -> options r
		 else options r
	  _ -> case break (==iac) s of
	         (plain,rest) -> putL plain $ options rest
      where getMore = getF $ \ s' -> options (s++s')



{-
-- This version operates on streams where the elements are single characters.
-- Unfortunately this is a bit too inefficient.

optionsF = ioF optionsK>=^^<concSP
  where
    iac=chr 255
    will=chr 251; wont=chr 252; do'=chr 253; don't=chr 254
    neg=(will,don't)
    names=["Will","Won't","Do","Don't"]
    putR s = putK [High (Left s)] -- output to remote
    putL s = putK [High (Right s)] -- output to local
    getR cont = getK $ \ msg -> case msg of High x -> cont x ; _ -> getR cont
    optionsK =
      getR $ \ c ->
	if c==iac then
	  getR $ \ cmd ->
	  if inRange neg cmd then
	    getR $ \ opt ->
	      --echoK (names !! index neg cmd ++ " " ++ show (ord opt)) $
	      if cmd==do' then putR [iac,wont,opt] optionsK
	      else if cmd==will then putR [iac,don't,opt] optionsK
	      else optionsK
	  else optionsK
	else putL [c] optionsK
-}
