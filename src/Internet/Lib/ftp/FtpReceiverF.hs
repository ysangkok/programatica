module FtpReceiverF(ftpDataF,FtpDataMsg(..)) where
import AllFudgets
import SocketK
import DialogueIO hiding (IOError)

data FtpDataMsg = FtpMsg String
                | FtpData String
		| FtpPort String

ftpDataF = ioF ftpReceiverK

ftpReceiverK =
  openLSocketK 0 (error "ftpReceiver cannot create a socket") $ \ lsocket ->
  getLSocketName lsocket $ \ s ->
  selectK [LSocketDe lsocket] $
  putFtpPort s $
  getConnectionK $ \ socket peer ->
  putFtpMsg ("Accepted connection from "++peer) $
  closeLSocketK lsocket $
  selectK [SocketDe socket] $
  getDataK0 $ \ s ->
  closeSocketK socket $
  putFtpAns s $
  ftpReceiverK

getConnectionK cont =
  getK $ \ msg ->
  case msg of
    Low (DResp (AsyncInput (_,SocketAccepted socket peer))) ->
      cont socket peer
    _ -> echoK "got something else" $ getConnectionK cont

getDataK0 cont =
    getDataK [] 0
  where
    getDataK acc n =
      getK $ \ msg ->
      case msg of
	Low (DResp (AsyncInput (_,SocketRead s))) ->
	  if null s
	  then cont (revconcat acc)
	  else let acc' = s:acc
		   n' = n + length s
	       in putFtpMsg ("Got "++show n'++" bytes") $
		  getDataK acc' n'
	_ -> getDataK acc n

revconcat = foldl (flip (++)) []

putFtpAns  x = putsK [High (FtpData x)]
putFtpMsg  x = putsK [High (FtpMsg x)]
putFtpPort s = putsK [High (FtpPort (take 2 s))]
