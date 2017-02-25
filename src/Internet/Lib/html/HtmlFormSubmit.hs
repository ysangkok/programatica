module HtmlFormSubmit(submitForm,HttpMethod) where
import URLencode(encode,encodeQuery)
import ParseURL(parseURL)
import URL(joinURL,relativeURL)
import Http(HttpMethod(..))
import Data.Maybe(fromMaybe)
import ListUtil(assoc)

submitForm (attrs,values) =
    case lookup "METHOD" attrs of
      Just "POST" -> (url,HttpPost encdata)
      _           -> (joinURL url query,HttpGet)
  where
    url = fromMaybe (relativeURL "") (lookup "ACTION" attrs >>= parseURL)
    query = relativeURL ("?"++encdata)
    encdata = case values of
                [("",s)] -> encode s
		_ -> encodeQuery values
