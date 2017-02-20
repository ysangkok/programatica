module DialogueSpIO where
import SP
import DoRequest
import DialogueIO(Response,Request(..))
import Queue

dialogueSpIO :: SP Response Request -> IO ()
{-
--Simple implementation:
dialogueSpIO sp =
    case sp of
      PutSP req sp' ->
        do resp <- doRequest req
	   dialogueSpIO (startupSP [resp] sp')
      GetSP xsp ->
        do resp <- doRequest GetAsyncInput
	   dialogueSpIO (xsp resp)
      NullSP -> return ()
-}

-- More efficient queueing of responses
dialogueSpIO sp = doIO sp empty
  where
    doIO sp respq =
      case sp of
        PutSP req sp' ->
          do resp <- doRequest req
	     doIO sp' (enter respq resp)
        GetSP xsp ->
	  case qremove respq of
	    Just (resp,respq') -> doIO (xsp resp) respq'
	    Nothing -> do resp <- doRequest GetAsyncInput
	                  doIO (xsp resp) respq
        NullSP -> return ()
