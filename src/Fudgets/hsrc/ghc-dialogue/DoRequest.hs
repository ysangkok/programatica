module DoRequest(initXCall,doRequest,getAsyncInput) where
import DialogueIO
import ContinuationIO(stdin,stdout,stderr)
import qualified IO
import Directory
import Time(getClockTime,toCalendarTime)

import DoXCommand
import DoXRequest
import AsyncInput(XCallState,initXCall,getAsyncInput',doSelect,doSocketRequest)
import CmdLineEnv(argFlag)

import System
import Prelude hiding (IOError)

import Ap

deb = argFlag "dorequest" False

doRequest =
    if not deb
    then doRequest'
    else \state req -> do
         eprint req
	 resp <- doRequest' state req
	 eprint resp
	 return resp
  where
    eprint x = IO.hPutStrLn IO.stderr . take 239 . show $ x

doRequest' :: XCallState -> Request -> IO Response
doRequest' state req =
  case req of
    ReadFile   filename          -> rdCatch (readFile filename)
    WriteFile  filename contents -> wrCatch (writeFile filename contents)
    AppendFile filename contents -> wrCatch (appendFile filename contents)
    StatusFile filename          -> catchIo SearchError (statusFile filename)
      where
        statusFile path =
	  do f <- doesFileExist path
	     if f then permissions 'f' path
               else do d <- doesDirectoryExist path
		       if d then permissions 'd' path
			  else fail path
	permissions t path =
	  do p <- getPermissions path
	     let r = if readable p then 'r' else '-'
		 w = if writable p then 'w' else '-'
	     return (Str [t,r,w])
    GetModificationTime path   -> catchIo SearchError (ClockTime # getModificationTime path)
    ReadDirectory dir          -> rdCatch' StrList (getDirectoryContents dir)
    DeleteFile filename        -> otCatch (removeFile filename>>return Success)
    CreateDirectory path mask  -> otCatch (createDirectory path>>return Success)
    ReadChan channelname ->
      if channelname==stdin
      then rdCatch getContents
      else return (Failure $ ReadError $
                   "ReadChan: unknown channel "++channelname)
    AppendChan channelname contents
        | channelname==stdout -> wr IO.stdout
        | channelname==stderr -> wr IO.stderr
        | otherwise           -> return (Failure $ WriteError ("AppendChan: unknown channel "++channelname))
      where wr chan = wrCatch (IO.hPutStr chan contents>>IO.hFlush chan)
    XRequest r      -> otCatch $ XResponse # doXRequest r
    XCommand c      -> otCatch $ (doXCommand c >> return Success)
    GetAsyncInput   -> getAsyncInput state
    SocketRequest r -> otCatch $ doSocketRequest state r
    Select dl       -> otCatch $ doSelect state dl
    Exit n	    -> exitWith (if n==0 then ExitSuccess else ExitFailure n)
    GetLocalTime    -> otCatch $ do
		         CalendarTime # (toCalendarTime =<< getClockTime)
			 --s <- readIO (formatCalendarTime undefined "%s" t)
			 --GHC bug(?) workaround:
                         --let s = ctSec t+60*(ctMin t+60*(ctHour t))
			 --return (Dbl (fromIntegral s))
    GetEnv var      -> catchIo SearchError (Str # getEnv var)
    System cmd      -> do exitcode <- system cmd
		          case exitcode of
		            ExitSuccess -> return Success
			    ExitFailure n -> return (Failure (OtherError ("System: Return code="++show n)))
    GetTime         -> otCatch $ ClockTime # getClockTime
    _ -> do IO.hPutStrLn IO.stderr msg
            return $ Failure $ OtherError $ msg
         where msg = "doRequest: unimplemented request: "++show req

getAsyncInput state = otCatch $ getAsyncInput' state

rdCatch = rdCatch' Str
rdCatch' c io = catchIo ReadError (c # io)
wrCatch io = catchIo WriteError (io >> return Success)
otCatch = catchIo OtherError
catchIo e io = catch io (return . Failure . e . show)

-- Should be put elsewhere:
#ifndef __GLASGOW_HASKELL__
instance Functor IO where map f io = io >>= (return . f)
#endif
