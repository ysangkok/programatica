module NewsFetchF(newsFetchF) where
import Fudgets
import NntpF
import URL(URL(..))
import MimeMessage(splitmsg,getheader)
import Dew(decodeEncodedWords)
import DecodeText(decodeText)
import Http
import HtmlEntities(encode)
import Data.Char(isSpace)

newsFetchF :: F URL (Either String (URL,Either String HttpResponse))
newsFetchF = loopThroughRightF (absF ctrlSP0) nntpF

ctrlSP0 = putStatusMsg "News fetcher starting..." ctrlSP

ctrlSP =
  getSP $ \ msg ->
  case msg of
    Right url@(URL _ _ _ id _) ->
      if '@' `elem` id
      then reqArticleById url ("<"++id++">")
      else case break (=='/') id of
             (_,"") -> listGroup url id
	     (grp,'/':arts) ->
	        case break (=='-') arts of
		  (art,"") -> reqArticleByNum url grp (read art)
		  (art1,'-':art2) -> listArticles url grp (read art1) (read art2)
    _ -> ctrlSP

reqArticleByNum url grp num =
  nntpCmd (GetArticle [Head,Body] grp num) $ getArticle url

reqArticleById url articleId =
  nntpCmd (GetArticleWithId [Head,Body] articleId) $ getArticle url

getArticle url resp =
  case resp of
    Article _ _ _ ls ->
      putAns url (HttpResp 200 "OK nttp" [("Content-Type","message/rfc822")] (unlines (decodeText ls)))
      ctrlSP
    NntpError s -> putErr url s ctrlSP

getGroup url groupName cont =
  nntpCmd (GetGroup groupName) $ \ resp ->
  case resp of
    Group (grp,cnt,first,last) -> cont first last
    NntpError s -> putErr url s ctrlSP

listGroup url grp =
  getGroup url grp $ \ first last ->
  listGroupArticles url grp first last (max first (last-20)) last

listArticles url grp wfirst wlast =
  getGroup url grp $ \ efirst elast ->
  listGroupArticles url grp efirst elast (max efirst wfirst) (min elast wlast)

listGroupArticles url grp efirst elast wfirst wlast =
  getHeaders grp wfirst wlast $ \ hdrs ->
  putAns url (articleList grp efirst elast wfirst wlast hdrs) $
  ctrlSP

articleList grp efirst elast wfirst wlast hdrs =
    HttpResp 200 "OK nntp" [("Content-Type","text/html")] $
    unlines $
    "<UL>":
      (if efirst<wfirst then ref "Earlier articles" (wfirst-20) (wfirst-1)
       else [])++
      map artref hdrs ++
      (if elast>wlast then ref "Later articles" (wlast+1) (wlast+21)
       else [])++
      ["</UL>"]
  where
    ref s f t =
      ["<LI><A HREF=news:"++grp++"/"++show f++"-"++show t++">"++s++"</A>"]
    artref (id,h) =
      "<LI><A HREF=\"news:"++aid++"\"><B>"++subj++" </B> </A> <I> "++auth++"</I>"
      where
        newsref u s ="<A HREF=\"news:"++u++"\">"++s++"</A>"
        headers = (fst.splitmsg) h
	hdr = encode.decodeEncodedWords.getheader headers
	subj = hdr "subject"
	auth = hdr "from"
	aid = trim id
	trim = reverse.trim2.reverse.trim1
	trim1 = dropWhile (\c->isSpace c || c=='<')
	trim2 = dropWhile (\c->isSpace c || c=='>')
	

getHeaders grp first last cont =
    get first []
  where
    get n acc =
      if n>last
      then cont (reverse acc)
      else nntpCmd (GetArticle [Head] grp n) $ \ resp ->
           case resp of
	     Article _ _ id ls -> get (n+1) ((id,ls):acc)
	     NntpError _ -> get (n+1) acc

putAns url s = putSP (Right (Right (url,(Right s))))
putStatusMsg = putSP. Right. Left
putErr url msg = putSP (Right (Right (url,(Left msg))))

nntpCmd cmd = cmdContSP (Left cmd) resp
  where
    resp (Left r) = Just r
    resp _ = Nothing
