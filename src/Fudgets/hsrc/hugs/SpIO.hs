module SpIO(spIO) where
import Command
import Event
import Loopthrough
import Path(Path(..))
import Cont(getRightSP)
import ShowFailure
import Sockets
import Spops
import SP(SP)
import Tables2
import Xtypes
import DialogueSpIO
import DialogueIO hiding (IOError)

spIO :: (SP (Path, Response) (Path, Request)) -> IO ()
spIO mainSP = dialogueSpIO (loopThroughRightSP tagRequestsSP mainSP)

tagRequestsSP = tagRequests dtable0
tagRequests dtable =
  getSP $ \msg ->
  case msg of
    Left (path', cmd) ->
      case cmd of
	Select ds -> let dtable' = updateDe path' ds dtable
		     in doReqSP (Select (listDe dtable')) $ \ resp ->
			checkErr resp (tagRequests dtable')
	XCommand _ -> doReqSP cmd $ \ resp ->
		      -- The response is to an XCommand is always Success
		      -- and is not propagated to the originating fudget.
		      tagRequests dtable
	_ -> doReqSP cmd $ \ resp ->
	     putSP (Left (path', resp)) $
	     tagRequests dtable
    Right ai@(AsyncInput (d, i)) ->
      putSP (Left (lookupDe dtable d, ai)) $
      tagRequests dtable

checkErr resp cont =
    case resp of
      Success -> cont
      Failure ioerr -> error ("IOerror: " ++ showFailure ioerr)

doReqSP req = putReqSP req . getRespSP
  where
    putReqSP = putSP . Right
    getRespSP = getRightSP
