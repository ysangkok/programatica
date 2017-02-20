module HtmlFuns where
import Html
import HtmlTags

--- HTML destruction: ---

extractTitle html =
  case extractElements TITLE html of
    HtmlContext (TITLE,_) t : _ -> Just (htmlchars t)
    _ -> Nothing

extractBase html =
  case extractElements BASE html of
    HtmlCommand (BASE,attrs) : _ -> lookup "HREF" attrs
    _ -> Nothing

extractBodyAttrs html =
  case extractElements BODY html of
    HtmlContext (BODY,attrs) _ : _ -> attrs
    _ -> []

extractElements t = extractElements' p
  where p (tag,_) = tag==t

extractElements' p = extr
  where
    extr = concatMap extr1

    extr1 e@(HtmlContext tag contents) =
      if p tag
      then [e]
      else extr contents
    extr1 e@(HtmlCommand tag) = if p tag then [e] else []
    extr1 _ = []

-- htmlchars -- removes the markup
htmlchars = concatMap chars
  where 
    chars (HtmlChars s) = s
    chars (HtmlContext _ html) = htmlchars html
    chars _ = ""

mapHtmlChars f = concatMap hmap
  where
    hmap i =
      case i of
        HtmlChars s -> f s
	HtmlContext tag html -> [HtmlContext tag (mapHtmlChars f html)]
	_ -> [i]

{-
mapHtmlTags f = map hmap
  where
    hmap item =
      case item of
        HtmlContext tag html -> HtmlContext (f tag) (mapHtmlTags f html)
	HtmlCommand tag -> HtmlCommand (f tag)
	_ -> item
-}

mapHtmlTags f = apHtmlElems cmd ctx
  where
    ctx tag html = HtmlContext (f tag) (mapHtmlTags f html)
    cmd tag = HtmlCommand (f tag)

apHtmlElems cmd ctx = map hmap
  where
    hmap item =
      case item of
        HtmlContext tag html -> ctx tag html
	HtmlCommand tag -> cmd tag
	_ -> item

apHtmlCtx = apHtmlElems HtmlCommand
apHtmlCmd cmd = apHtmlElems cmd HtmlContext

rmHtmlGarbage = concatMap rm
  where
    rm item =
      case item of
        HtmlGarbage _ -> []
	HtmlContext tag html -> [HtmlContext tag (rmHtmlGarbage html)]
	_ -> [item]
