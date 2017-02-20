module URLFetchF(urlFetchF,HttpReq(..),HttpResp(..)) where
import AllFudgets
import FtpF
import Http
import HttpFetchF
import NewsFetchF
import URL(URL(..),url2str)
import List(sort)
import Maybe(fromMaybe)
import DialogueIO hiding (IOError)

urlFetchF :: F HttpReq HttpResp
urlFetchF = snd >^=< listF fetchers >=^< distr
  where
    fetchers =
      [("file",fileFetchF>=^<reqURI),
       ("http",httpFetchF), -- also handles gopher & finger
       ("news",delayF newsFetchF>=^<reqURI),
       ("telnet",telnetStarterF>=^<reqURI)
      ]
    distr req@(HttpReq {reqURI=url}) = (fetcher,req)
      where
        fetcher =
	  case url of
	    URL (Just proto) _ _ _ _ ->
	      if proto `elem` map fst fetchers
	      then proto
	      else case proto of
		"ftp"  -> "file"
		_      -> "http"
	    -- _ -> "http" -- !!

telnetStarterF = ioF tsK
  where
    putMsg s = putsK [High (Left s)]
    tsK =
      getK $ \ msg ->
      case msg of
        High (URL _ opthost optport path _) ->
	    putMsg ("Starting "++cmd++". Log in as "++user++".") $
	    hIOSucc (System (cmd++" &")) $
	    tsK
	  where cmd = "xterm -e telnet "++host++" "++port
	        host = fromMaybe "localhost" opthost
	        port = case optport of
		         Just p -> show p
			 _ -> ""
		user = case path of
		         '/':s -> s
			 _ -> path
	_ -> tsK

fileFetchF = loopThroughRightF (ioF ctrlK0) (delayF ftpF)
  where
    ctrlK0 =
      getK $ \ msg ->
      case msg of
        High (Right url@(URL _ opthost optport path _)) ->
	    if host=="localhost" && port==21 
	    then putMsg ("Reading local file "++path) $
	         getFileK path errK $ \ ans ->
	         putAns url ans $
		 ctrlK0
	    else put1K (Left ((host,port),path)) $
	         ctrlK url
	  where port = fromMaybe 21 optport
	        host = fromMaybe "localhost" opthost
		errK = const $ putErr url ("Can't read "++path) $ ctrlK0
	_ -> ctrlK0
    ctrlK url =
      getK $ \ msg ->
        case msg of
          High (Left (Left s)) -> putMsg ("ftp: "++s) (ctrlK url)
	  High (Left (Right (Right ans))) -> putAns url ans ctrlK0
	  High (Left (Right (Left err))) -> putErr url err ctrlK0
	  _ -> putMsg ("Ignoring new request. "++url2str url++" in progress.") $
	       ctrlK url

    putMsg msg = putsK [(High. Right. Left) msg{-,Low Flush-}]
    put1K = putHigh
    
    putErr url msg = put1K (Right (Right (url,Left msg)))

    putAns url (typ,file) = put1K (Right (Right (url',Right (httpResp typ file))))
      where url' = if typ/="FILE"
                   then appSlash url
		   else url

    appSlash (URL proto host port path fragment) =
      let path' = if last path=='/'
                  then path
		  else path++"/"
      in URL proto host port path' fragment

    httpResp typ file = HttpResp 200 "OK file" [("Content-Type",typ)] file

getFileK path errK cont =
    hIOerr (StatusFile path) errK $ \(Str s) ->
    case s of
      'd':_ ->
        hIOerr (ReadDirectory path) errK $ \ (StrList ls) ->
	cont ("NLST",unlines (sort ls))
      _ ->
        hIOerr (ReadFile path) errK $ \ (Str s) ->
	cont ("FILE",s)
