module HtmlParser (parseHtml)
where
import Fudgets(argFlag)
import Html
import HtmlTags
import HtmlLex
import ParsOps
import HtmlEntities(collapseSpace)
import HtmlPrinter(printTag,printHtml)
import Data.ListSet(intersect)
import Data.ListUtil(mapFst)
import Data.Char(isSpace)

parseHtml :: String -> Either (Error Char) Html
parseHtml s =
   case parse htmlDocument ((lexClean.htmlLex) s) of
     Right html -> Right html
     Left (es, ts) -> Left (es, showHtmlLex ts)

htmlDocument =
  unit (\h hs ts->h:hs++[HtmlChars (showHtmlLex ts)]) `chk`
  whitespace `ap`
  optCtx HTML (unit (:) `ap` optHeader `ap` optBody) `ap`
  html `ap` -- some documents have stuff after </HTML> !!
  endgarb `chk`
  eof

endgarb = if argFlag "garb" True
          then unit []
	  else many (lit Just)

optHeader = whitespace `cap` optCtx HEAD htmlHeader
optBody   = whitespace `cap` some (optCtx BODY htmlBody)
            -- there are documents with several bodies !!

htmlHeader = whitespace `cap` many (headerElement `chk` whitespace)
  where
    headerElement =
      ctx [TITLE] (many chars)
        `orelse`
      ctx [SCRIPT] whitespace
        `orelse`
      cmd [ISINDEX,NEXTID, LINK, BASE,META]
        `orelse`
      comment
  
optCtx t p = opttag t `ap` p `chk` optendtag t
  where
    opttag s = HtmlContext `mapP` optional (s,[]) (tag [s])
    optendtag s = optional s (endtag s)

cmd ns = HtmlCommand `mapP` tag ns

ctx ns p =
  tag ns `bind` \ ctx@(name,_) ->
  unit (HtmlContext ctx) `ap` p `chk` endtag name


anytag =
  lit (\t -> case t of
               HtmlTag t -> Just t
	       _ -> Nothing)

tag ns = anytag `bind` chktag
  where
    chktag t@(n,_) =
      if n `elem` ns
      then unit t
      else failP ("unexpected tag: "++show n)

anyendtag = lit (\t -> case t of
                        HtmlEndTag n -> Just n
			_ -> Nothing)

endtag n = anyendtag `bind` chktag
  where
    chktag n' =
      if n'==n
      then unit n
      else failP ("got end tag "++show n'++" instead of "++show n)

htmlBody = some item
html = many item

item = form `orelse` plainItem
  where
    form = ctx [FORM] formContents
    formContents = many formItem
    plainItem = command `orelse` context `orelse` chars `orelse` garbage
    command = cmd commands
    context = preElem `orelse` ctx contexts html
    preElem = ctx [PRE] pre
    pre = many preItem
    preItem = prechars `orelse` command `orelse` preContext `orelse` preGarbage
    preContext = ctx contexts pre
    preGarbage = (\g@(HtmlGarbage _) -> HtmlChars (printHtml [g])) `mapP` garbage

    formItem = input `orelse` select `orelse` textArea `orelse` fplain
      where
        input = cmd [INPUT]
	select = ctx [SELECT] (many option `chk` whitespace)
	option = whitespace `cap` ctx [OPTION] (many chars)
	textArea = ctx [TEXTAREA] (many prechars)
	fplain = command `orelse` fcontext `orelse` chars `orelse` garbage
	fcontext = preElem `orelse` ctx contexts formContents

garbage = comment `orelse` badtag --`orelse` unknownTag
{-
  where
    unknownTag =
      HtmlGarbage `mapP`
        (anytag `filterP` ((`notElem` okBodyTags).fst))
          `orelse`
      (\s->HtmlGarbage ('/':s,[])) `mapP`
        (anyendtag `filterP` (`notElem` okEndTags))
    okEndTags = BODY:HTML:FORM:SELECT:TEXTAREA:contexts
-}

{-
garbage = comment `orelse`
          anytag `bind` garbageTag `orelse`
	  anyendtag `bind` garbageEndTag `orelse`
	  badtag
  where
    garbageEndTag n =
      if n `notElem` okEndTags
      then unit (HtmlGarbage ('/':n,[]))
      else  failP ("misplaced tag: "++n) -- unlikely to occur
    garbageTag t@(n,_) | n `notElem` okBodyTags = unit (HtmlGarbage t) -- !!
    garbageTag (n,_) = failP ("misplaced tag: "++n) -- unlikely to occur
    okEndTags = BODY:HTML:FORM:SELECT:TEXTAREA:contexts
-}

comment = badtag

whitespace = many (comment `orelse` space)

space = HtmlChars `mapP` (entities `filterP` all isSpace)

badtag =
  lit (\t->case t of
             HtmlBadTag s -> Just (HtmlGarbage (s,[]))
	     _ -> Nothing)

htmlchars = HtmlChars. collapseSpace

chars = htmlchars `mapP` entities
prechars = HtmlChars `mapP` entities

entities = lit (\t->case t of
                      HtmlEntities s -> Just s
		      HtmlSpace s -> Just s
		      _ -> Nothing)


-- commands & contexts constructed from  "HTML Quick Reference"
-- URL http://www.ncsa.uiuc.edu/General/Internet/WWW/HTMLQuickRef.html

commands =
  [BR,P,IMG,LI,DD,DT,HR,ISINDEX,META,AREA] -- !! AREA only in MAPs

contexts =
  [PRE,LISTING,PLAINTEXT,BLOCKQUOTE,
   A,
   DL,UL,OL,MENU,DIR,
   CENTER, -- netscape extension
   FUPPLET, -- WWWBrowser extension
   TABLE,TR,TH,TD,CAPTION,
   MAP, -- client side image maps
   ADDRESS] ++
  headers ++
  highlighting

headers = [H1 .. H6]

highlighting =
  [EM,STRONG,CODE,SAMP,KBD,VAR,DFN,CITE, -- logical
   FONT, -- netscape extension
   SMALL,BIG,  -- HTML 3.2 physical
   B,I,U,TT] -- physical

formCtxs = [FORM,SELECT,TEXTAREA,OPTION]
formCommands = [INPUT]
formtags = formCommands ++ formCtxs

okBodyTags = contexts ++ commands ++ formtags

lexClean = properNest [] . filter endTag
  where
    endTag (HtmlEndTag s) = s `notElem` commands
    endTag _ = True

    properNest stack [] = map HtmlEndTag stack
    properNest stack (h:hs) =
      case h of
        HtmlTag (n,_) | n `elem` nestContexts ->
	  case tagsExcludedBy n of
	    [] -> h:properNest (n:stack) hs -- optimisation of special case
	    es -> case excludeSplit es stack of
	            (ts,stack') ->
		      map HtmlEndTag ts++[h]++properNest (n:stack') hs
	HtmlEndTag n | n `elem` nestContexts ->
	  case stack of
	    (t:ts) | n==t -> h:properNest ts hs
	           | otherwise ->
		       case break (==n) ts of
		         (_,[]) -> badtag
			 (is,_:os) -> map HtmlEndTag (t:is)++[h]++
			              properNest os hs

		     | otherwise -> badtag
	    [] -> badtag
	  where badtag = HtmlBadTag ('/':show n):properNest stack hs        
	_ -> h:properNest stack hs

    nestContexts = [HTML,HEAD,BODY] ++ formCtxs ++ contexts

    tagsExcludedBy n =
      case n of
        A -> [A]
	P -> [A,H1,H2,H3,H4,H5,H6]
	          -- won't work, since P isn't treated as a context
	UL -> [A]
	OL -> [A]
	OPTION -> [n]
	BODY -> [HEAD]
	FORM -> [n]
	--FONT -> [FONT]
	--These can be nested if tables are nested:
	--TH -> [TH,TD]
	--TD -> [TH,TD]
	--TR -> [TR,TH,TD]
        _ -> []


excludeSplit es = head.filter (null.intersect es.snd).splits

splits [] = [([],[])]
splits xxs@(x:xs) = ([],xxs):mapFst (x:) (splits xs)
    
