module DoRequest(doRequest) where
import Prelude hiding (IOError)
import DialogueIO
import ContinuationIO(stdin,stdout,stderr)
import qualified IO
--import Directory
import XCall
import System

doRequest :: Request -> IO Response
doRequest req =
  case req of
    ReadFile   filename          -> rdCatch (readFile filename)
    WriteFile  filename contents -> wrCatch (writeFile filename contents)
    AppendFile filename contents -> wrCatch (appendFile filename contents)
    --ReadDirectory dir            -> rdCatch' StrList (getDirectoryContents dir)
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
    XRequest _      -> doXCall req
    XCommand _      -> doXCall req
    GetAsyncInput   -> doSCall req
    SocketRequest _ -> doSCall req
    Select _        -> doSCall req
    Exit n	    -> exitWith (if n==0 then ExitSuccess else ExitFailure n)
    _ -> return $ Failure $ OtherError
	   ("doRequest: unimplemented request: "++show req)

rdCatch = rdCatch' Str
rdCatch' f io = catch (fmap f $ io) (return . Failure . ReadError . show)


wrCatch io =
  catch (io >> return Success) (return . Failure . WriteError . show)
    

-- Should be put elsewhere:
--instance Functor IO where map f io = io >>= (return . f)
