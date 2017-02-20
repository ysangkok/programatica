module ImagesF(imagesF) where
import Fudgets
import InsetsF
import ImageF
import Html
import HtmlTags
import ImageFetchF

imagesF = insetsF imgsrcs imageFetchF imageF

imgsrcs = concatMap extr
  where
    extr e =
      case e of
        HtmlCommand (IMG,attrs) -> [(lookup "SRC" attrs,attrs)]
	HtmlContext _ html -> imgsrcs html
	_ -> []
