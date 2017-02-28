module InsetsF(insetsF) where
import Fudgets
import UserLayoutF
import ParseURL(parseURL)
import URL(URL(..),url2str,joinURL)
import Html
import ListMap(lookupWithDefault)

insetsF extract fetchF showF =
    loopThroughRightF (swapEither>^=<dynF nullF >=^< pre) fetchF
  where
    pre (Right (Left imgs)) = Left (dynInsetsF imgs)
    pre (Right (Right places)) = Right (Left places)
    pre (Left px) = Right (Right px)

{-
    dynInsetsF :: (URL,Html) -> F (Either [Rect] (Int,(blaha,(Size,PixmapId))))
                                  (Either [Size] (Int,URL))
-}
    dynInsetsF (url,html) =
        (collectLayoutF n.listF.number 0.map (showF url)) is
      where is = extract html
            n = length is
