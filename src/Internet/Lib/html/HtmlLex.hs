module HtmlLex(module HtmlLex) where
import Html(HtmlTag(..),TagAttrs(..))
import HtmlTags
import HtmlPrinter(printTag)
import HtmlEntities(decode)
import RichTextLex
import AnchorParser(parseTag)
import qualified Data.Map.Strict as OM
import Utils2(isSpace')
--import LexSymbol

data HtmlLex = HtmlEntities String
             | HtmlSpace String
	     | HtmlBadTag String
             | HtmlTag HtmlTag
	     | HtmlEndTag TagName

cmpLex l1 l2 =
    case (l1,l2) of
      (HtmlTag (t1,_),HtmlTag (t2,_)) -> compare t1 t2
      (HtmlEndTag t1,HtmlEndTag t2) -> compare t1 t2
      _ -> compare (pos l1) (pos l2)
  where
    pos (HtmlEntities _) = 0
    pos (HtmlSpace _) = 1
    pos (HtmlBadTag _) = 2
    pos (HtmlTag _) = 3
    pos (HtmlEndTag _) = 4


instance Eq HtmlLex where l1==l2 = compare l1 l2==EQ
instance Ord HtmlLex where compare = cmpLex
instance Show HtmlLex where showsPrec n l = (sh1 l++)

{-
instance Symbol HtmlLex where lexToInt = lex2int
lex2int = toInt
  where
    firstendtag = 4+fromEnum (maxBound::TagName)
    toInt l =
      case l of
	HtmlEntities _ -> 0
	HtmlSpace _ -> 1
	HtmlBadTag _ -> 2
	HtmlTag (t,_) -> 3+fromEnum t
	HtmlEndTag t ->  firstendtag + fromEnum t
-}

{-
-- This can not be derived:
instance Ix HtmlLex where
  index (lo,hi) i = lex2int i-lex2int lo
  inRange (lo,hi) i = lex2int lo<=lex2int i && lex2int i<=lex2int hi
-}

-- Characteristic elements from each equvalence class:
htmlEntities = HtmlEntities "plain text"
htmlSpace = HtmlSpace "space"
htmlBadTag = HtmlBadTag "bad tag"
htmlTag t = HtmlTag (t,[])
htmlEndTag = HtmlEndTag

htmlAny = [htmlEntities,htmlSpace,htmlBadTag]++
	  map htmlTag allTags++
          map htmlEndTag allTags

htmlLex = cleanup . map rt2html . rtlex
  where
    rt2html rt =
      case rt of
        Chars s ->
	  if all isSpace' s
	  then HtmlSpace s
	  else HtmlEntities (decode s)
	FmtCmd s1@('/':s) ->
	  case parseTag s of
	    Just (n,_) ->
	      case tagLex n of
	        Just t -> HtmlEndTag t
		_ -> HtmlBadTag s1
	    _ -> HtmlBadTag s1
	FmtCmd s ->
	  case parseTag s of
	    Just (n,attrs) ->
	      case tagLex n of
		Just t -> HtmlTag (t,attrs)
		_ -> HtmlBadTag s
	    _ -> HtmlBadTag s
        Comment s -> HtmlBadTag ("!--"++s++"--")

    cleanup [] = []
    cleanup (HtmlEntities "":ls) = cleanup ls
    cleanup (HtmlSpace "":ls) = cleanup ls
    cleanup (HtmlTag t:l:ls) = HtmlTag t:cleanup (rmnl l:ls)
    cleanup (l1:l2@(HtmlEndTag t):ls) =
	case rmlastnl l1 of
	  HtmlEntities "" -> cleanup (l2:ls)
	  HtmlSpace "" -> cleanup (l2:ls)
	  l1' -> l1:cleanup (l2:ls)
    cleanup (HtmlEndTag BODY:ls) = cleanup ls -- these only cause trouble
    cleanup (HtmlEndTag HTML:ls) = cleanup ls
    cleanup (l:ls) = l:cleanup ls

    rmnl = mapchars dropnl
    rmlastnl = mapchars (reverse . dropnl .reverse)
    dropnl ('\n':s) = s ; dropnl s = s
    mapchars f (HtmlEntities s) = HtmlEntities (f s)
    mapchars f (HtmlSpace s) = HtmlSpace (f s)
    mapchars f l = l

tagLex = lookup
  where
    lookup name = OM.lookup name tags
    tags = OM.fromList ([(show t,t) | t <- allTags])
{-
-- This solution is only marginally faster:
  lookup name =
    case name of
      "TITLE" -> Just TITLE
      ...
      "ISINDEX" -> Just ISINDEX
      "FUPPLET" -> Just FUPPLET
      _ -> Nothing
---}

showHtmlLex = concatMap sh1

sh1 l =
      case l of
        HtmlEntities s -> s
        HtmlSpace s -> s
	HtmlTag t -> printTag t
	HtmlEndTag s -> "</"++show s++">"
	HtmlBadTag s -> "<"++s++">"
