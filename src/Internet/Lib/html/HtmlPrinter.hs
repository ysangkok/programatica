module HtmlPrinter(printHtml,printTag) where
import Html
import HtmlEntities(encode)
import Data.Char(isAlpha,isDigit)

printHtml :: Html -> String
printHtml = concatMap printItem
  where
    printItem i =
      case i of
        HtmlCommand tag -> printTag tag
	HtmlContext tag@(name,attrs0) html ->
	    case attr1 of
	      "implicit" -> printHtml html
	      "implicitend" -> printTag (name,attrs) ++ printHtml html
	      _ -> printTag tag ++ printHtml html ++ printEndTag tag
	  where
	    (attr1,attrs) = case attrs0 of
	                      (a,_):attrs -> (a,attrs)
			      _ -> ("",attrs)

--	HtmlChars s -> s  -- escape special chars !?
	HtmlChars s -> encode s -- yes
	HtmlGarbage tag -> printGarb tag

printTag :: HtmlTag -> String
printTag = pt ""
printEndTag (n,_) = pt "/" (n,[])
--printGarb (n,as) = pt' "?" n as
printGarb (n,as) = pt' "" n as

pt s (n,as) = pt' s (show n) as
pt' s n as = "<"++s++n++printAttrs as++">"
  where
    printAttrs = concatMap printAttr
    --printAttr (n,"") = " "++n
    --printAttr (n,v) | n==v = " "++n
    printAttr (n,v) = " "++n++"="++optquote v

    optquote "" = quote ""
    optquote v = if all isNoQuote v then v else quote v

    isNoQuote c = isAlpha c || isDigit c || c=='.' || c=='-'

    quote s = q++s++q
    q = ['"']
