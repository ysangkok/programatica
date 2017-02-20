module socket where 

import AllFudgets
import lconnect
import DialogueIO
import Prelude hiding (IOError)

data SocketMsg a = SocketMsg a | SocketEOS
data ServerMsg a = ServerMsg a | ServerEOS | ServerNew

aSocketMsg :: (a -> b) -> SocketMsg a -> SocketMsg b
aSocketMsg f (SocketMsg a) = SocketMsg (f a)
aSocketMsg f SocketEOS = SocketEOS

--and socketServer :: Port -> (Socket -> F a (SocketMsg b))
--                    -> F (Int , a) (Int , ServerMsg b)
socketServer ports f = 
  let
      router e =
         let todyn = Left . Right
             out = Right
         in
            case e of
             -- from control
	       Left (Left (i,f)) -> [todyn (i,DynCreate f), out (i,ServerNew)]
             -- from dynListF
	       Left (Right (i,m)) -> 
		 case m of
		    SocketMsg m' -> [out (i,ServerMsg m')]
		    SocketEOS    -> [out (i,ServerEOS), todyn (i,DynDestroy)]
             -- from outside
	       Right (i,m) -> [todyn (i,DynMsg m)]

      control = lconnect ports $ (\lsocket->
         let accepter i = 
	       getMessageFu $ (\e-> 
	       case e of
		  Low (DResp (AsyncInput (_,SocketAccepted socket peer)))->
		    putF (i,f i socket peer) $
		    accepter (i+1)
	          _ -> accepter i
	       {-end-})
	 in
	     putMessagesFu [Low (DReq (Select [LSocketDe lsocket]))] $
	     (accepter 0))
  in loopLeftF (idRightF (control >+< dynListF) 
                >=^^< concmapSP router)
      

