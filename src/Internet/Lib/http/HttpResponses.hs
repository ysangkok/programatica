module HttpResponses where
import Http
import MimeTypes(htmlType)
import MimeMessage(updateHdr)

okResponse	 = okResponse' htmlType
badQueryResponse = badQueryResponse' htmlType
notFoundResponse msg = notFoundResponse' htmlType
			("<TITLE>Not Found</TITLE>\n"++
			 "<H1>Not Found</H1>\n"++msg)
notImplementedResponse = notImplementedResponse' htmlType
serverErrorResponse = stdResponse 500 "Internal Server Error" htmlType

okResponse'	  = stdResponse 200 "Have a Fudget"
notFoundResponse' = stdResponse 404 "Not found"
badQueryResponse' = stdResponse 400 "Error in query or submission"
notImplementedResponse' = stdResponse 501 "Not implemented"
serverErrorResponse' = stdResponse 500 "Internal Server Error"

stdResponse code msg typeHeaders body =
  HttpResp code msg
           (("Server",serverId):
            ("Mime-Version","1.0"):
            (typeHeaders::HttpHeaders))
           body
  where
    serverId = "HttpResponses/0 InternetLib (internetlib@mail.altocumulus.org)"

addServer serverId resp =
    resp{respHdrs=updateHdr ("Server",serverId) (respHdrs resp)}
