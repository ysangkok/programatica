module HtmlParsOps where
import Html
import HtmlLex
import ParsOps3
--import HtmlEntities(collapseSpace)

tag = mapP htag . oneOf . map htmlTag
tag1 =  mapP htag . tok . htmlTag
endtag = tok . htmlEndTag

htag (HtmlTag tag) = tag

anythingButCtx n =
    HtmlContext `mapP` tag1 n `ap` many (anythingBut end) `chk` endtag n
  where
    end = htmlEndTag n
    anythingBut n = (HtmlChars . sh1) `mapP` oneOf [ t | t<-htmlAny,t/=n]


cmd = mapP HtmlCommand . tag
cmd1 = mapP HtmlCommand . tag1

ctx ns p = foldl1 orelse (map (flip ctx1 p) ns)

ctx1 n p = HtmlContext `mapP` tag1 n `ap` p `chk` endtag n

ctx' ns p = foldl1 orelse (map (flip ctx1' p) ns)

ctx1' n p =
    HtmlContext `mapP` tag1 n `ap` p `chk` optendtag n
  where
    optendtag s = optional undefined (endtag s)

impliedCtx t p = opttag t `ap` p `chk` optendtag t
  where
   opttag s = HtmlContext `mapP` optional (s,[]) (tag1 s)
   optendtag s = optional undefined (endtag s)
    --opttag s = HtmlContext `mapP` (tag1 s `err` ((s,[]),["Insert "++show s]))
    --optendtag s = endtag s `err` (undefined,[""])

--chars = htmlchars `mapP` entities
chars = HtmlChars `mapP` entities
--htmlchars = HtmlChars . collapseSpace
--prechars = HtmlChars `mapP` entities

entities = hchars `mapP` oneOf [htmlEntities,htmlSpace]
--entities = (str1 . hchars) `mapP` tok htmlEntities `orelse` (str2 . hchars) `mapP` tok htmlSpace
  where
    hchars (HtmlSpace s) = s
    hchars (HtmlEntities s) = s

comment = badtag

badtag = mapP garb (tok htmlBadTag)
  where garb (HtmlBadTag t) = HtmlGarbage (t,[])

space = many space1

space1 = mapP space (tok htmlSpace)
  where space (HtmlSpace s) = HtmlChars s

whitespace = many whitespace1
whitespace1 = comment `orelse` space1
