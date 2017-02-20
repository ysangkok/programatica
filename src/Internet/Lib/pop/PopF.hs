module PopF(PopRequest(..),PopResponse(..),popTalkF) where
import Fudgets
import IOUtil(getEnvi)
import Pop
import Base64(encodeBase64)

data PopResponse = Headers [(Int,(Int,[String]))]
                 | Msg [String]
	         | Err String
		 | PopQuit
		 deriving (Show) -- for debugging

data PopRequest = GetMsg Int
                | QuitPop
		deriving (Show) -- for debugging

popTalkF =
  loopThroughRightF (popF (absF popInitSP)) (snd>^=<passwdPopupF "")

out = Right . Right

type PopSP = PopClientSP (Either PasswdResp PopRequest) (Either PasswdReq PopResponse)
type PasswdResp = String
type PasswdReq = (Maybe String,Maybe String)

popInitSP :: PopSP
popInitSP =
  getOkResp $ \ _ ->
  popCmd "CAPA" $ \ (msg:capas) ->
  if "SASL LOGIN" `elem` capas
  then saslLoginSP
  else popLoginSP

popLoginSP =
  popCmd ("USER "++user) $ \(msg:_) ->
  getPasswd msg $ \ passwd ->
  popCmd ("PASS "++passwd) $ \_ ->
  popListSP

saslLoginSP =
  putPopCmd "AUTH LOGIN" $
  checkPop "+ VXNlciBOYW1lAA==" $
  putPop (encodeBase64 user) $
  checkPop "+ UGFzc3dvcmQA" $
  getPasswd ("POP password for "++user++", please") $ \ passwd ->
  putPop (encodeBase64 passwd) $
  getOkResp $ \ _ ->
  popListSP

popListSP =
  popCmd "LIST" $ \(_:msgs) ->
  getHeadersSP [] (map ((\ (s1:s2:_)->(read s1::Int,read s2::Int)) . words) (reverse msgs))

getHeadersSP msgs [] = putSP (out (Headers (reverse msgs))) getMsgSP
getHeadersSP msgs ((n,size):ns) =
    if n `mod` 4 == 0
    then putSP (out (Headers (reverse msgs))) $
         popCmd ("TOP "++show n++" 0") $ \(resp:msg) ->
         getHeadersSP ((n,(size,msg)):[]) ns
    else popCmd ("TOP "++show n++" 0") $ \(resp:msg) ->
         getHeadersSP ((n,(size,msg)):msgs) ns

{-
resparg1 resp =
  case words resp of
    _:s:_ ->
      case reads s of
	[(x,"")] -> x
	_ -> 0 -- ??
-}

getMsgSP =
  getSP $ \msg->
  case msg of
   Right (Right (GetMsg n)) ->
     popCmd ("RETR "++show (n::Int)) $ \msg ->  -- !! Shouldn't die on error!
     putSP (out (Msg msg)) $
     getMsgSP
   Right (Right QuitPop) ->
     quitSP
   _ -> getMsgSP

quitSP =
     popCmd "QUIT" $ \ msg ->
     putSP (out PopQuit) nullSP

checkPop expected cont =
  getPop $ \ s ->
  if s==expected
  then cont
  else errSP ("Expected "++expected++", POP server said "++s)

checkResp s cont =
  case s of
    ('+':_):_ -> cont s
    msg:_     -> errSP msg

getOkResp cont = getPop $ \ s -> checkResp [s] cont

errSP msg = putSP (out (Err msg)) getMsgSP -- output error msg and stop

getPasswd msg cont =
  let expect (Right (Left passwd)) = Just (Just passwd)
      expect (Right (Right QuitPop)) = Just Nothing
      expect _             = Nothing
  --in cmdContSP (Right (Left [Left msg])) expect $ \p ->
  in cmdContSP (Right (Left (Just msg,Nothing))) expect $ \p ->
     case p of
       Just passwd -> cont passwd
       Nothing        -> quitSP

popCmd cmd = doPopCmd cmd . flip checkResp

defaultUser=
  case getEnvi "USER" of
    Just user -> user
    _ -> error "Who are you?"

user = argKey "user" defaultUser
