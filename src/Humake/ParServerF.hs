module ParServerF where
import Fudgets

parServerF ss serverF = loopThroughRightF (absF ctrlSP0) serversF
  where
    nss = number 1 ss
    ns = map fst nss
    serversF = listF [(i,serverF n)|(i,n)<-nss]

    ctrlSP0 = ctrlSP ns []

    ctrlSP servers queue =
	case (servers,queue) of
	  (s:ss,req:reqs) -> putSP (Left (s,req)) $ ctrlSP ss reqs
	  _               -> getSP $ either fromServerSP requestSP
      where
	requestSP req = ctrlSP servers (queue++[req])

        fromServerSP (s,msg) = either statusMsgSP answerSP msg
	  where
	    statusMsgSP status =
              putSP (Right (Left (s,status))) $
	      ctrlSP servers queue
	    answerSP ans =
	      putSP (Right (Right ans)) $
	      --ctrlSP (s:servers) queue -- stack of servers
	      ctrlSP (servers++[s]) queue -- fifo of servers
	      -- With a fifo, the fastest server is likely to get more jobs.
{- -- alternate version:
	        case queue of
		  req:reqs ->
		    -- Start a new compilation BEFORE outputting the result.
		    putSP (Left (s,req)) $
		    putAns $
		    ctrlSP servers reqs
		  _ -> putAns $ ctrlSP (s:servers) queue
	       where
		 putAns = putSP (Right (Right ans))
---}
