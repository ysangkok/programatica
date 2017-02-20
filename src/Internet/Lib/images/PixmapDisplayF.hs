module PixmapDisplayF(pixmapDisplayF,pixmapDisplayF',PixmapId) where
import AllFudgets
import Maybe(maybeToList,isJust)
--import Maybe(fromMaybe)

#ifdef __HASKELL98__
#define map fmap
#endif

pixmapDisplayF = pixmapDisplayF' Nothing

pixmapDisplayF' :: Maybe Size -> F (Size,PixmapId) a
pixmapDisplayF' optSize =
    showCommandF "pix" $
    windowF ll0 $ k
  where
    ll0 = maybeToList (map ll optSize) -- This is what I want
    --ll0 = [ll (fromMaybe 20 optSize)] -- this avoid a problem with autoLayoutF
    k = getK $ \msg -> 
	case msg of
	  High (size,pixmap) ->
	    putsK (map Low (ll1 size++
			    [XCmd $ ChangeWindowAttributes [CWBackPixmap pixmap],
			     XCmd $ ClearWindow])) k
	  _ -> k

    ll1 = if isJust optSize then const [] else (:[]) . ll
    ll s = layoutRequestCmd $ plainLayout s True True
