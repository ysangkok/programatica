module Http(module Http,Headers) where
import MimeMessage(hdrlines,Headers,MimeMessage(..),showMessage)
--import Base64(encodeBase64)
import Utils2(addcr)

data HttpRequest uri
  = HttpReq {	reqURI::uri,
		reqMethod::HttpMethod,
		reqHdrs::HttpHeaders }

data HttpMethod
   = HttpGet
   | HttpHead
   | HttpPost String
   | HttpPostFormData [(String,String,String)]
   deriving (Eq,Show)

type HttpHeaders = Headers

data HttpResponse
  = HttpResp {	respCode::Int,
		respMsg::String,
		respHdrs::HttpHeaders,
		respBody::String }

showHttpResponse = showHttpResponse' addcr

showHttpResponse' addcr (HttpResp code msg hdrs body) =
    addcr (unlines (statusline:hdrlines hdrs++[""])) ++ body
  where
    statusline = unwords ["HTTP/1.0",show code,msg]

showHttpRequest showURI (HttpReq uri method hdrs) =
    addcr ( unlines (
    unwords [mname,showURI uri,"HTTP/1.0"]:
    hdrlines (hdrs++extrahdrs) ++
    [""])) ++
    body
  where
    (mname,extrahdrs,body) =
      case method of
        HttpGet    -> ("GET",[],[])
        HttpHead   -> ("HEAD",[],[])
	HttpPost s ->
	  ("POST",
	   [("Content-type","application/x-www-form-urlencoded"),
	    ("Content-length",show (length bodycr))],
	    bodycr)
          where bodycr = addcr s
	HttpPostFormData formdata ->
	  ("POST",
	   [("Content-Type","multipart/form-data; boundary="++boundary),
	    ("Content-length",show (length bodycr))],
            bodycr)
          where
            bodycr =
              multipart ("--"++boundary) (map encodePart formdata)
    boundary = "formdata-f61040bde459e26fae9b27f5219fa37e"
    encodePart (name,value,attr) =
      MimeMsg [("Content-Disposition","form-data; name="++name++attr){-,
               ("Content-Transfer-Encoding","base64")-}]
               ({-encodeBase64-} value++crlf)

    multipart boundary parts =
      "This is a multi-part message in MIME format"++crlf++
      concatMap (((boundary++crlf)++) . showMessage) parts++
      boundary++"--"++crlf

crlf="\r\n"
