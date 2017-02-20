module TransceiverF(transceiverF) where
import AllFudgets hiding (transceiverF)
import SocketK
import DialogueIO hiding (IOError)
import Utils2(ord)

transceiverF :: F (Either (Host,Port) String) String
transceiverF = ioF idleK

idleK =
  getK $ \ msg ->
  case msg of
    High (Left (host,port)) -> connectK host port
    _ -> echoK "ignoring input while unconnected" $
         idleK

connectK host port =
    openSocketK host port errK $ \ socket ->
    getSocketName socket $ \ s ->
    --echoK ("Connected to "++host) $
    putsK [High ("220-"++show (map ord (take 4 (drop 2 s)))++"\n")] $
    selectK [SocketDe socket] $
    transceiverK socket
  where
    errK ioerror = putsK [High ("500 "++show ioerror)] idleK

transceiverK socket =
  loop $ \ same ->
    getK $ \ msg ->
    case msg of
      High (Left (host,port)) ->
	closeSocketK socket $
        connectK host port
      High (Right s) -> writeSocketK socket s same
      Low (DResp (AsyncInput (_,SocketRead s))) ->
        if null s
	then closeSocketK socket $
	     idleK
	else putsK [High s] same
      _ -> same
