module UserLayoutF2 where
import AllFudgets
import Sort(sortLe)

type UE a = Either [Rect] a
type UC b = Either [Size] b
type UF a b = F (UE a) (UC b)

collectLayoutF :: Int -> F a b -> UF a b
collectLayoutF n (F fud) = F (loopThroughRightSP sizeSP0 fud)
  where
    sizeSP0 =
      trace "collectLayoutF says hello" $
      sizeSP' []
    -- first collect and output the sizes of the n fudgets
    sizeSP' ts =
      if length ts == n
      then let ts' = sortLe (\(t1,_) (t2,_) -> t1<=t2) ts
               (tags,sizes) = unzip ts'
               smsg = (Right. High. Left) sizes
	   in putSP smsg $ placeSP tags
      else sizeSP ts
    sizeSP ts =
      getSP $ \ msg ->
      case msg of
	Right (High (Left ps)) -> sizeSP ts -- can't happen
	Left (Low (tag,LayoutLimits ll)) ->
	  case ll of
	    LayoutRequest (Layout {minsize=size}) ->
	      let ts' = replace (tag,size) ts
	      in trace ("got "++show (length ts')++" of "++show n) $
	         sizeSP' ts'
	    _              -> sizeSP ts -- Could propagate MakeVisible...
	_ -> commonSP msg $ sizeSP ts

    -- then wait for the placement list
    placeSP tags =
      getSP $ \ msg ->
      case msg of
	Right (High (Left ps)) -> putsSP (zipWith pl tags ps) $ placeSP tags
	  where pl tag r = Left (Low (tag,LayoutPlace r))
	Left (Low (tag,LayoutLimits ll)) ->
	  trace "ignoring updated LayoutLimits" $
	  placeSP tags
		-- Ignores re-layout requests.
		-- Better to repeat last placement, since e.g. editorF
		-- expects to always get a response to a layout request.
	_ -> commonSP msg $ placeSP tags

    commonSP msg =
      case msg of
	Right (High (Right x)) -> putSP (Left (High x))
	Left (High x)       -> putSP (Right (High (Right x)))
	Right (Low  te)      -> putSP (Left (Low te))
	Left (Low tcmd)     -> putSP (Right (Low tcmd))

trace = ctrace "cldebug"
