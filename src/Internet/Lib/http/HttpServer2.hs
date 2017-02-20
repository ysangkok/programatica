module HttpServer2(httpServerF{-,httpServerFromInetdF-},CalendarTime) where
import Fudgets
import ContribFudgets(socketServerF,SocketMsg(..),ClientMsg(..))
--import URL
import Http
--import DialogueIO hiding (IOError)
--import ContinuationIO(stdout)
import Data.ListUtil(breakAt)
import Time(CalendarTime,calendarTimeToString)
import Utils2(strToLower)
import MimeMessage(splitmsg)

-- For servers started from inetd with the "wait" option:
{-
httpServerFromInetdF =
  error "httpServerFromInetdF not implemented"
  -- httpServerF' False (LSo 0)
-}

httpServerF = httpServerF' True
    
httpServerF' log port =
    loopThroughRightF serverHandlerF (socketServerF port clientHandlerF)
  where
    serverHandlerF = absF handlerSP
      where
        handlerSP = getSP $ either fromClientSP (const handlerSP)

	fromClientSP (n,ClientMsg msg) =
	  putSP (Right msg) $
	  getRightSP $ \ ans ->
	  putSP (Left (n,ans)) $
	  handlerSP
	fromClientSP _ = handlerSP

    clientHandlerF socket peer =
        getLocalTime $ \ t ->
	loopThroughRightF (clientF t) (stdoutF>+<asyncTransceiverF socket)
      where
        toLog = Left . Left
	toClient = Left . Right
	toServer = Right

        logSP msg = if log then putSP (toLog msg) else id

        clientF t = absF clientSP
	  where
	    clientSP =
	        logSP (peer++" ["++calendarTimeToString t++"] ") $
		getQuerySP0 $ either badQuerySP $ \ q ->
		logSP (logQuery q) $
		putSP (toServer (SocketMsg (peer,t,q))) $
		getRightSP $ \ ans ->
		putSP (toClient ans) $
		logSP ((unlines.take 1.lines) ans) $
		putSP (toClient "") $ -- eos, close socket
		getEosSP
	      where
		badQuerySP ls =
		  putSP (toClient errormsg) $
		  logSP "query not understood" $
		  getEosSP
		errormsg = "HTTP/1.0 400 Bad Request\n\nQuery not understood"

            getEosSP =
	      getSP $ \ msg ->
	      case msg of
	        Left (Right "") -> putSP (toServer SocketEOS) nullSP
		_ -> getEosSP

	    getQuerySP0 = getQuerySP ""
	      where
		getQuerySP q cont =
		  getSP $ \ msg ->
		  case msg of
		    Left (Right s) ->
		      let q'=q++(filter (/='\r') s)
			  ql = lines q'
		      in if s=="" || "" `elem` ql
			 then cont (parseHttpReq ql)
			 else getQuerySP q' cont
		    _ -> getQuerySP q cont

parseHttpReq ql@(l1:ls) =
  case words l1 of
    ms:path:_ -> Right (HttpReq path method hdrs)
      where
        (hdrs,body) = splitmsg ls
	method =
	  case strToLower ms of
	    "get" -> HttpGet
	    "post" -> HttpPost (unlines body)
	    "head" -> HttpHead
    _ -> Left ql


logQuery =
  if argFlag "logrequests" False
  then showHttpRequest id
  else unlines . take 1 . lines . showHttpRequest id
