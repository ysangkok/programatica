module HttpServer(httpServerF,httpServerFromInetdF) where
import AllFudgets
--import URL
import Http
import DialogueIO hiding (IOError)
import ContinuationIO(stdout)
import ListUtil(breakAt)
import Time(calendarTimeToString)
import Utils2(strToLower)
import Message(splitmsg)
import SocketK

-- For servers started from inetd with the "wait" option:
httpServerFromInetdF = httpServerF' False (LSo 0)

httpServerF port = 
    openLSocketF port $ httpServerF' True
    
httpServerF' log lsocket =
    loopThroughRightF (ioF clientK0) (ioF serverK0)
  where

    logK = if log then logToStdoutK else noLogK
    logToStdoutK = appendChanK stdout
    noLogK s cont = cont

    serverK0 =
      selectK [LSocketDe lsocket] $
      serverK

    serverK =
      getK $ \ msg ->
      case msg of
	Low (DResp (AsyncInput (_,SocketAccepted socket peer))) ->
	  getLocalTime $ \ t ->
	  logK (peer++" ["++calendarTimeToString t++"] ") $
	  putK (High (socket,peer)) $
	  serverK
	_ -> logK "got something else" $
	     serverK

    clientK0 =
	clientK
      where
	clientK =
	  getK $ \ msg ->
	  case msg of
	    High (Left (socket,peer)) ->
		selectK [SocketDe socket] $
		getQueryK0 $ either badQueryK $ \ q ->
		logK (logQuery q) $
		--selectK [] $ -- ?
		getLocalTime $ \ t ->
		serviceK (peer,t,q) $ \ ans ->
		writeSocketK socket ans $
		closeSocketK socket $
		logK ((unlines.take 1.lines) ans) $
		clientK
	      where
		badQueryK ls =
		  writeSocketK socket errormsg $
		  closeSocketK socket $
		  logK "query not understood"
		  clientK
		errormsg = "HTTP/1.0 400 Bad Request\n\nQuery not understood"
	    _ -> clientK

getQueryK0 = getQueryK ""
  where
    getQueryK q cont =
      getK $ \ msg ->
      case msg of
	Low (DResp (AsyncInput (_,SocketRead s))) ->
	  let q'=q++(filter (/='\r') s)
	      ql = lines q'
	  in if s=="" || last ql=="" -- POST does not work because of this
	     then cont (parseHttpReq ql)
	     else getQueryK q' cont
	_ -> getQueryK q cont -- BUG: new connections are thrown away!!!

serviceK q = cmdContMsg (High (Right q)) ans
  where ans (High (Right a)) = Just a
        ans _ = Nothing

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

{- -- A better implementation is now part of the Fudget library.
getLocalTime cont = hIO GetLocalTime $ \ (Dbl t) -> cont (convTime t)


-- stolen from FileStat.hs
convTime t = addToClockTime (TimeDiff{tdYear = 0, tdMon = 0, tdDay = 1, tdHour = 0, tdMin = 0, tdSec = s, tdPicosec = ps}) clockTime1970
  where
    (s,fr) = properFraction t
    ps = floor (fr*1.0e12)
    clockTime1970 = toClockTime (CalendarTime{ctYear=1970, ctMon=January, ctDay=1, ctHour=0, ctMin=0, ctSec=0, ctPicosec=0, ctTZName="", ctTZ=0, ctIsDST=False})
-}
