module HttpRequest where
import Http(HttpRequest(..),HttpMethod(..))
import MimeMessage(rmcrHdr,rejoin,disasmHeader)
import Utils2(strToLower)

parseHttpRequest s =
  case words l1 of
    ms:path:_ -> Right (HttpReq path method hdrs)
      where
	method =
	  case strToLower ms of
	    "get" -> HttpGet
	    "post" -> HttpPost body
	    "head" -> HttpHead
    _ -> Left s
  where
    (l1,ls) = case rejoin (lines hs) of
                [] -> ([],[])
                l1:ls -> (l1,ls)
    hdrs = map disasmHeader ls
    (hs,body) = rmcrHdr s
