module MimeMessage where -- message and message header manipulation functions
import Data.List(intersperse)
import Data.Char(isSpace)
import Utils2(strToLower,addcr)
import HO(apFst,apSnd)
import Data.ListUtil(breakAt)
import Data.ListMap(lookupWithDefault)

data MimeMessage body = MimeMsg { mimeHdrs :: Headers, mimeBody :: body }
                      deriving Show
type Headers = [(String,String)]

parseMessage :: String -> MimeMessage String
parseMessage =
  uncurry MimeMsg . apFst (map disasmHeader . rejoin . lines) . rmcrHdr

showMessage (MimeMsg hdrs body) = asmMsg (hdrs,body)

splitmsg = apFst (map disasmHeader) . breakAt "" 

disasmHeader = apSnd rmspace . breakAt ':'

getheader headers h =
  dropWhile isSpace (lookupWithDefault (map (apFst strToLower) headers) "" (strToLower h))

getmsgheader msg h = getheader (fst (splitmsg msg)) h

mimeContentType = fst . contentType

contentType hdrs =
  apFst strToLower (breakAt ';' (getheader hdrs "Content-Type"))

asmHeader (h@('F':'r':'o':'m':' ':_),r) = h ++ ":" ++ r
asmHeader (h,r) = h ++ ": " ++ r

hdrlines = map asmHeader
asmMsg (hdrs,body) = addcr (unlines (hdrlines hdrs)) ++ "\r\n" ++ body

renameHdr from to' = map (apFst (\h -> if h==from then to' else h))
             -- to is a keyword

updateHdr (h,r) [] = [(h,r)]
updateHdr (h,r) ((h',r'):hdrs) =
	if h==h'
	then (h',r):hdrs
	else (h',r'):updateHdr (h,r) hdrs

modHdr h f =
  let modhdr (h',r) = (h',if strToLower h'==h then f r else r)
  in map modhdr

parseMsg = splitmsg . lines -- old
msglines (hdr,body) = hdrlines hdr ++ [""] ++ body
printMsg = unlines . msglines

rmspace (' ':s) = s
rmspace s = s


rmcrHdr ('\r':s) = rmcrHdr s
rmcrHdr ('\n':'\r':'\n':s) = ("",s)
rmcrHdr ('\n':'\n':s) = ("",s)
rmcrHdr (c:s) = apFst (c:) (rmcrHdr s)
rmcrHdr "" = ("","")

rejoin (l1:ls) =
  case break (null . takeWhile isSpace) ls of
    (ls1,ls2) -> concat (intersperse "\n" (l1:ls1)):rejoin ls2
rejoin [] = []
