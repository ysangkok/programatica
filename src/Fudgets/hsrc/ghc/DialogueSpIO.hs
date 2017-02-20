module DialogueSpIO where
import SP
import DoRequest(initXCall,doRequest,getAsyncInput)
import DialogueIO(Request(XCommand))
import Queue

--dialogueSpIO :: SP FResponse FRequest -> IO ()
{-
--Old, simple implementation:
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
dialogueSpIO sp = do
  iostate <- initXCall
  let doIO sp respq =
	case sp of
	  PutSP req sp' ->
	    do resp <- doRequest iostate req
	       case req of
		 -- The response to an XCommand is always Success
		 -- and is not propagated to the originating fudget.
	         XCommand {} -> doIO sp' respq
		 _ -> doIO sp' (enter respq resp)
	  GetSP xsp ->
	    case qremove respq of
	      Just (resp,respq') -> doIO (xsp resp) respq'
	      Nothing -> do resp <- getAsyncInput iostate
			    doIO (xsp resp) respq
	  NullSP -> return ()
  doIO sp empty
