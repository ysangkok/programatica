module AnchorParser where
import ParsOps
import HtmlEntities(decode)
import Utils2(strToUpper)
import Char(isSpace,isAlphaNum)

parseTag s =
    case parse tag s of
      Right a -> Just a
      _ -> Nothing
  where
    tag = unit (,) `ap` tagname `ap` many attribute `chk` space `chk` eof
    attribute = attr `mapP` attrname `ap` optvalue
    attr n Nothing = (n,n)
    attr n (Just v) = (n,v)
    optvalue = maybeP (kw "=" `cap` value)
    value = (decode `mapP` string) `orelse` optional "" name

    tagname = (tagnamefix.strToUpper) `mapP` name
    attrname = strToUpper `mapP` name
    name = space `cap` some (scan isNameChar)
    space = many (scan isSpace)
    string = stringq '\'' `orelse` stringq '"'
    stringq q = space `cap` tok q `cap` many (scan (/=q)) `chk` tok q
    kw s = space `chk` toks s

    tagnamefix "HEADER" = "HEAD"
    tagnamefix name = name
    isNameChar c = isAlphaNum c || c `elem` "~'%/:.-?&#_@+" -- what is allowed?
