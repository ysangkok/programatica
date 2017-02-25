module HttpFetchF(httpFetchF,HttpResp(..),HttpReq(..)) where
import AllFudgets
import ParseURL(parseURL)
import URL(URL(..),url2str,joinURL,docURL)
import Http
import HttpResponse
import MimeMessage(getheader,updateHdr)
import SocketK
import DialogueIO hiding (IOError)
import IOUtil(getEnvi) -- !! not standard Haskell
import Data.Char(toLower)
import HO(apSnd)
import ListUtil(chopList,breakAt)
import Utils2(strToLower)
import Data.Maybe(fromMaybe)
import qualified PackedString as PS

#ifdef __HBC__
#define lengthPS length
#endif

type HttpResp = Either String (URL,Either String HttpResponse)
type HttpReq = HttpRequest URL
{-
type HttpSP sp = sp HttpRequest HttpResp
type HttpF = HttpSP F	-- Ha!
type HttpK = HttpSP K	-- Ha!
-}

type HttpF = F HttpReq HttpResp
type HttpK = K HttpReq HttpResp
type ErrK = String->HttpK
type SuccK = HttpResponse->HttpK

httpFetchF :: HttpF
httpFetchF = ioF httpFetchK

httpFetchK :: HttpK
httpFetchK =
  putStatusMsg () "Ready" $
  httpFetchK'

httpFetchK' :: HttpK
httpFetchK' =
  getK $ \ msg ->
    case msg of
      High req ->
          fetchUrlK req errK succK
	where
	  succK ans =
	    putAns (reqURI req) ans $
	    httpFetchK'
	  errK err =
	    putErr (reqURI req) err $
	    httpFetchK'
      _ -> httpFetchK
  where
    putAns url ans = putRes url (Right ans)
    putErr url msg = putRes url (Left msg)
    putRes url ans = putK (High (Right (url,ans)))

fetchUrlK :: HttpReq -> ErrK -> SuccK -> HttpK
fetchUrlK = fetchUrlK' []

fetchUrlK' from httpReq@(HttpReq { reqURI=url }) errK succK =
  case url of
    URL (Just proto) opthost optport path _ ->
      case strToLower proto of
        "http" ->
	    proxySendQueryK httpReq' url (host,port,path) errK $ \ ans ->
	    followRedirectionK from url httpReq' (parseHttpResponse ans) errK succK
	  where
	    port = fromMaybe 80 optport
	    hdrs = reqHdrs httpReq
	    agentHdr = ("User-Agent","HttpFetchF/0 (http://www.cs.chalmers.se/~hallgren/)")
	    httpReq' = httpReq { reqHdrs = hdrs' }
	    hdrs' =
	      (if getheader hdrs "User-Agent"=="" then (agentHdr:) else id) $
	      updateHdr ("Host",host++maybe "" ((':':).show) optport)
	      hdrs

	"gopher" ->
	    sendQueryK host port (gopherQuery path) url errK $ \ ans ->
	    succK (httpResp "gopher" ans)
	  where port = fromMaybe 70 optport
	"finger" ->
	    sendQueryK host port (fingerQuery path) url errK $ \ ans ->
	    succK (httpResp "finger" ans)
	  where port = fromMaybe 79 optport
		-- finger is not part of the URL standard
	_ -> errK ("Protocol "++proto++" is not implemented yet.")
      where
        host = fromMaybe "localhost" opthost
	httpResp proto body = HttpResp 200 ("OK "++proto) [] body
    _ -> errK "Can't retrieve documents from incomplete URLs"

followRedirectionK from url httpReq ans errK succK =
  case respCode ans of
    301 -> follow "permanent"
    302 -> follow "temporary"
    _ -> dontfollow
  where
    loc = "Location"
    dontfollow = succK ans'
      where ans' = ans {respHdrs = updateHdr (loc,url2str url) (respHdrs ans)}
    follow descr =
      case parseURL (getheader (respHdrs ans) loc) of
	Just url' | aurl `notElem` from && length from<20 ->
	    putStatusMsg url' ("Following "++descr++" redirection") $
	    fetchUrlK' (url:from) httpReq {reqURI=aurl} errK succK
          where aurl=joinURL url url'
	_ -> dontfollow


proxySendQueryK =
    case apSnd reads $ breakAt ':' proxy of
      (proxyhost,[(proxyport,"")]) -> proxySendQueryK' proxyhost proxyport
      _ -> noProxySendQueryK
  where
    noProxySendQueryK httpReq url (host,port,path) =
        sendQueryK host port (showHttpRequest (const path') httpReq) url
      where path' = if null path then "/" else path

    proxySendQueryK' proxyhost proxyport httpReq url u@(host,_,_) =
      if host `elem` noproxyhosts
      then noProxySendQueryK httpReq url u
      else sendQueryK proxyhost proxyport (showHttpRequest (url2str . docURL) httpReq) url


--sendQueryK :: String -> Int -> String -> URL -> Cont HttpK String
sendQueryK host port query url errK succK =
    putStatusMsg url ("Connecting to "++host) $
    openSocketK host port errK1 $ \ socket ->
    putStatusMsg url ("Sending request to "++host) $
    writeSocketK socket query $
    (if showreq then echoK query else id) $
    selectK [SocketDe socket] $
    putStatusMsg url ("Waiting for response from "++host) $
    waitReplyK [] 0 socket url errK succK
  where
    errK1 ioerror = errK ("http: "++show ioerror)

--waitReplyK :: [PackedString] -> Int -> Socket -> URL -> Cont HttpK String
waitReplyK acc n socket url errK succK =
  getK $ \ msg ->
  case msg of
    High req ->
      -- If a new request arrives, forget about the one in progress
      closeSocketK socket $
      startupK [High req] $ errK ("Aborting the fetch of "++url2str url)
    Low (DResp (AsyncInput (_,SocketRead s))) ->
      if null s
      then closeSocketK socket $
	   putStatusMsg url ("Got all "++show n++" bytes") $
           succK (revconcat acc)
      else let ps = PS.packString s
	       acc' = ps:acc
               n' = n +
	            PS.lengthPS ps
           in seq n' $ -- This also evaluates ps and gets rid of s.
	      putStatusMsg url ("Got "++show n'++" bytes") $
	      waitReplyK acc' n' socket url errK succK
    _ -> waitReplyK acc n socket url errK succK

--revconcat = foldl (flip (++)) []
revconcat = concatMap PS.unpackPS . reverse

gopherQuery path =
    drop 2 (map qt path) ++ "\n\n" -- drops a "/" and the type character
    -- query shouldn't be URLencoded. !!
  where
    qt '?' = '\t'
    qt c   = c

fingerQuery path = drop 1 path ++ "\n\n" -- drops the leading "/"

putStatusMsg url msg = putsK [High (Left msg){-,Low Flush-}]

proxy = argKey "proxy" httpProxy
  where httpProxy = fromMaybe "" (getEnvi "HTTP_PROXY")

noproxyhosts = argKeyList "noproxy" []

showreq = argFlag "showhttpreq" False
