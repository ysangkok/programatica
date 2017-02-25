module SimpleHttpServer(
  simpleHttpServer,
  Host,Port,SimpleHttpRequest,CalendarTime,
  SimpleHttpResponse,replyWith,replyWithFile,saveThen,
  txtType,htmlType,contentType,
  okResponse,okResponse',
  badQueryResponse,badQueryResponse',
  notFoundResponse,notFoundResponse',
  notImplementedResponse,notImplementedResponse',
  stdResponse,
  getQuery,getSimpleQuery
 ) where
import Fudgets
import HttpServer2
import Http
import URL hiding (Host,Port)
import ParseURL
import URLdecode
import MimeTypes
import HttpResponses
import ListUtil(chopList,breakAt)
import HO(apSnd)
import DialogueIO hiding (IOError)

type SimpleHttpRequest = (Host,CalendarTime,HttpRequest String)
newtype SimpleHttpResponse =
  R [(Either (Either (FilePath,String) FilePath) HttpResponse)]
unR (R r) = r

replyWith =  R. (:[]) . Right
replyWithFile =  R . (:[]) . Left . Right
saveThen s (R r) = R $ Left (Left s):r

simpleHttpServer :: (state->SimpleHttpRequest->(state,SimpleHttpResponse)) ->
		    state -> Port -> IO ()
simpleHttpServer serverFunc initState port = fudlogue simpleHttpServerF
  where
    simpleHttpServerF = loopF (fileF>==<serverF>==<httpServerF port)
    serverF = absF (mapstateSP serverFunc' initState)
      where serverFunc' s r = apSnd unR (serverFunc s r)

    --httpServerF' = maybe httpServerFromInetdF httpServerF optPort

    fileF = showHttpResponse . stripEither >^=< idRightF fileIOF

    fileIOF = filterRightSP >^^=< (writeFileF>+<fileF')
              -- add some error handling!

    writeFileF = contMap wr
      where wr (file,contents) = hIOF (WriteFile file contents)

    fileF' = post >^=< readFileF 
      where
	post (path,res) =
	  case res of
	    Right s -> simple (okResponse' (typeHdrs path) s)
	    Left err -> simple (notFoundResponse (show err))

--------------------------------------------------------------------------------

getQuery :: HttpRequest String -> Maybe (FilePath,[(String,String)])
getSimpleQuery :: HttpRequest String -> Maybe (FilePath,String)

getQuery = getQuery' decodeQuery
getSimpleQuery = getQuery' decode

getQuery' dec HttpReq{reqURI=urlstr,reqMethod=method} =
  do url <- parseURL urlstr
     let path = urlPath url
         ok path query = return (decode path,dec query)
     case method of
       HttpGet -> case break (=='?') path of
		    (path','?':query) -> ok path' query
		    _ -> ok path "" -- fail instead?
       HttpPost query -> ok path query
       _ -> Nothing
--------------------------------------------------------------------------------
simple = addServer serverId
serverId = "SimpleHttpServer/0 HttpServer (hallgren@cs.chalmers.se)"
