-- Client

import Fudgets
import TypedSockets
import Port

main = fudlogue $ shellF "Calendar" client

host = argKey "host" "localhost"

address = tServerAddress host port

client = (menu,Above) >#+< trans address

menufs = [("Quit",quitF)]

menu = listF menufs >==< 
       marginHVAlignF 0 aLeft aCenter 
       ((\x->(x,())) >^=< menuF "File" (map (\(s,f)->(s,s)) menufs))

days = ["Måndag","Tisdag","Onsdag","Torsdag","Fredag"]
times = [8..16]

dummydt = ("",0)

fields = (dummydt,nullLF) : map timelabel times ++ concat
         [daylabel day : [((day,time),field) | time <- times] | day <- days]

timelabel t = label aRight (show t)
daylabel d = label aCenter d
label al s = (dummydt,labelF' (setAlign al) s)

field = filtersameF (\i->inputLeaveDoneSP >^^=< strF i) ""

strF def = startupF [def] $ stringF' (setSizing Dynamic)

filtersameF f init = loopCompThroughLeftF (f init >+< absF (filterSP init))
   where filterSP msg = getSP $ \m -> 
			case m of

			     Left msg' -> if msg == msg' then filterSP msg
					 else putsSP [Right msg'] $
					      filterSP msg'

			     Right msg' -> putsSP [Left msg'] $ filterSP msg'

tr a = post >^=< tTransceiverF a
   where post (SocketMsg s) = s
	 post SocketEOS  = error "The server died!"

matrix = listLF (tableP' (length times+1) Vertical 2) fields
trans a = loopF (tr a >==< matrix)
