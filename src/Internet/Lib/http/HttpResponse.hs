module HttpResponse(parseHttpResponse) where
import Http
import MimeMessage(MimeMessage(..),parseMessage)

parseHttpResponse :: String -> HttpResponse
parseHttpResponse s =
    case headers of
      [] -> HttpResp 200 "OK" [] body -- pre HTTP/1.0 server
      h1@(h,h'):hdrs ->
	  case words h of
	    version:code:msgws ->
	        case reads code of
		  [(c,"")] -> HttpResp c msg hdrs body
		  _ -> HttpResp 200 msg hdrs body -- ??
	      where msg = unwords msgws ++ ": "++h'
	    _ -> HttpResp 200 "No result code. OK?" (h1:hdrs) body -- ??
  where
    MimeMsg headers body = parseMessage s
