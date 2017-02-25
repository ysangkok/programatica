module CacheF where
import Fudgets
import ListUtil(assoc)

cacheF serverF = loopThroughRightF (absF (cacheSP [] [])) serverF

cacheSP cache pending =
    getSP $ either answerFromServerSP requestFromClientSP
  where
    requestFromClientSP (n,req) =	-- A request from client n.
        assoc oldSP newSP cache req
      where
        oldSP ans =	-- The answer was found in the cache.
	    putSP (Right (n,(req,ans))) $
	    cacheSP cache pending

	newSP =		-- A new request, send it to the server and
			-- add the add client to the pending list.
	    if req `elem` map snd pending
	    then cont
	    else putSP (Left req) cont
	  where
	    cont = cacheSP cache ((n,req):pending)

    answerFromServerSP ans@(req,_) =
		-- The server delivered an answer to request req,
		-- save it in the cache,
		-- forward it to waiting clients and remove them from
		-- the pending list.
        putsSP [Right (n,ans) | (n,_)<-ready] $
	cacheSP (ans:cache) pending'
      where
        (ready,pending') = part ((==req).snd) pending
