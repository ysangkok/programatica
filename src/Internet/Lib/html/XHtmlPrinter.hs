module XHtmlPrinter(printHtml,printTag) where
import Html
import HtmlEntities(encode)
import Char(isAlpha,isDigit)
import Utils2(strToLower)
import HO(apFst)

printHtml = concatMap printItem
  where
    printItem i =
      case i of
        HtmlCommand tag -> printEmptyTag tag
	HtmlContext tag@(name,attrs0) html ->
	    if attr1=="implicit"
	    then printHtml html -- hmm
	    else printTag tag' ++ printHtml html ++ printEndTag tag'
	  where
	    tag' = if attr1 `elem` ["implicit","implicitend"]
		   then (name,attrs)
		   else tag
	    (attr1,attrs) = case attrs0 of
	                      (a,_):attrs -> (a,attrs)
			      _ -> ("",attrs)

--	HtmlChars s -> s  -- escape special chars !?
	HtmlChars s -> encode s -- yes
	HtmlGarbage tag -> printGarb tag

printEmptyTag (n,as) = pt'' "" " /" (show n) as
printTag = pt ""
printEndTag (n,_) = pt "/" (n,[])
--printGarb (n,as) = pt' "?" n as
printGarb (n,as) = pt'' "!--" "--" n as

pt s (n,as) = pt' s (show n) as
pt' pre n as = pt'' pre "" n as
pt'' pre post n as = "<"++pre++strToLower n++printAttrs as++post++">"
  where
    printAttrs = concatMap (printAttr . apFst strToLower)
    printAttr (n,"") = " "++n++"="++quote n
    printAttr (n,v) = " "++n++"="++quote v

    quote s = q++s++q
    q = ['"']
