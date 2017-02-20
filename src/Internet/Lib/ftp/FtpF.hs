module FtpF(ftpF) where
import AllFudgets
import FtpReceiverF
import FtpControlF
import SocketK
import DialogueIO hiding (IOError)
import Utils2(ord)

ftpF = loopThroughRightF (ioF ftpK) (ftpControlF>+<ftpDataF)

type FtpK = K (Either (Either FtpResponse FtpDataMsg) ((Host,Port),String))
              (Either (Either FtpRequest  ())         (Either String Ans))

type Ans = Either String (String,String) -- errormsg or (file type,contents)

ftpK :: FtpK
ftpK =
  getReq $ \ (ftpserv,path) ->
  ftpConnectAndGetK ftpserv path $
  ftpConnectedK ftpserv

ftpConnectedK ftpserv addr =
  getReq $ \ (ftpserv',path) ->
  if ftpserv'==ftpserv
  then ftpCmd "NOOP" $ \ reply ->
       -- did connection time out?
       case replyCode reply of
         "200" -> ftpGetK addr path $
	          ftpConnectedK ftpserv addr
	 _ -> ftpConnectAndGetK ftpserv' path $
	      ftpConnectedK ftpserv'
  else ftpConnectAndGetK ftpserv' path $
       ftpConnectedK ftpserv'

ftpConnectAndGetK ftpserv path cont =
  putFtpMsg ("Opening ftp connection to "++fst ftpserv) $
  ftpConnectK ftpserv $ \ reply ->
  case replyCode reply of
    '5':_ -> putErrAns (unwords (tail reply)) ftpK
    _ -> let addr = extrAddr reply
         in hIO (GetEnv "USER") $ \ (Str user) ->
	    ftpLoginK "ftp" (user++"@") $
	    ftpGetK addr path $
	    cont addr

ftpLoginK user passwd cont =
  ftpSuccCmd ("USER "++user) $
  ftpSuccCmd ("PASS "++passwd) $
  ftpSuccCmd "TYPE I" $
  cont

ftpGetK addr path cont =
  getDataPort $ \ p ->
  ftpSuccCmd ("PORT "++portstring (addr++map ord p)) $
  ftpCmd ("RETR "++path) $ \ reply ->
  case replyCode reply of
    "550" ->
       ftpCmd ("LIST "++path) $ \ reply ->
         case replyCode reply of
	   "150" -> getFile $ \ s ->
	            putOkAns "LIST" s $
		    cont
	   _ -> putErrAns (unlines reply) cont
    "150" -> getFile $ \ s ->
             putOkAns "FILE" s $
	     cont
    _ -> putErrAns (unlines reply) cont

getFile :: (String -> FtpK) -> FtpK
getFile cont =
  getFtpReply $ \ reply ->
  getDataData $ \ s ->
  cont s
  
ftpConnectK (host,port) =
  putFtpReq (FtpConnect host port)

ftpSuccCmd cmd cont =
  ftpCmd cmd $ \ msg ->
  putFtpMsg (unwords msg) $
  cont

ftpCmd cmd = putFtpReq (FtpCommand cmd)

putFtpReq ftpreq =
    putsK [High (Left (Left ftpreq))] . getFtpReply

getFtpReply =
    waitForK resp
  where
    resp (High (Left (Left s))) = Just s
    resp _ = Nothing

getReq =
    waitForK req
  where
    req (High (Right r)) = Just r
    req _ = Nothing

getDataPort cont =
  getData $ \ d ->
  case d of
    FtpMsg msg -> putFtpMsg msg $ getDataPort cont
    FtpPort p -> cont p

getDataData cont =
  getData  $ \ d ->
  case d of
    FtpMsg msg -> putFtpMsg msg $ getDataData cont
    FtpData s -> cont s

getData =
    waitForK dat
  where
    dat (High (Left (Right x))) = Just x
    dat _ = Nothing

putFtpMsg :: String -> FtpK -> FtpK
putFtpMsg msg = --echoK msg .
                putsK [High (Right (Left msg))]

putAns ::  Ans -> FtpK -> FtpK
putAns ans = putsK [High (Right (Right ans))]

putOkAns typ s = putAns (Right (typ,s))
putErrAns msg = putAns (Left msg)

extrAddr ((_:_:_:'-':s):_) = read s ::[Int]

replyCode reply = take 3 (last reply)
 -- ftpControlF prepends stuff, so take last line to get the real response code

portstring = tail . concatMap ((',':).show)
