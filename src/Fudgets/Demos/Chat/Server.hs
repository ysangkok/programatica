-- Server

import Fudgets
import TypedSockets
import ListMap(lookupWithDefault)
import Port

main = fudlogue server

server = 
  let broadcast cl = getSP $ \(i,e) ->
                      let clbuti = filter ((/= i) . fst) cl
		          bc s cs ncs = let msg = (lookupWithDefault (cs++ncs) "?" i,s)
					in putsSP [(i,msg) | (i,_) <- cs] $
					   broadcast ncs
                      in case e of
			    ClientNew -> broadcast cl
			    ClientMsg (Right (peer,name))  -> 
			     bc ("connected from "++ peer) cl ((i,name):cl)
			    ClientMsg (Left s)  -> bc ("says "++ s) cl cl
			    ClientEOS  -> bc "has quit" cl clbuti

  in loopF (broadcast [] >^^=< tSocketServerF port trans)

trans peer transceiver = concmapSP post >^^=< transceiver
 where
   post msg = case msg of
                SocketMsg (I_am name) -> [SocketMsg (Right (peer,name))]
		SocketMsg (Msg m) -> [SocketMsg (Left m)]
		SocketEOS -> [SocketEOS]

