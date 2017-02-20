module MGOps where
import MeasuredGraphics(MeasuredGraphics(..))
import Maptrace(ctrace)

{-
mgPart drawing path =
  case path of
    [] -> drawing
    p:ps -> part drawing
      where
        part drawing =
	  case drawing of
	    LeafM _ _ _     -> error "bad path in mgPart"
	    SpacedM   _  d  -> part d
	    PlacedM   _  d  -> part d
	    ComposedM    ds -> mgPart (ds !! ((p::Int)-1)) ps
-}

{-    
replaceMGPart drawing path new =
  (if any (<1) path
  then ctrace "replaceMGPart" path
  else id) $ replaceMGPart' drawing path new
-}

replaceMGPart drawing path new =
  case path of
    [] -> new
    p:ps  -> repl drawing
      where
        err = error ("bad path in replaceMGPart: "++show path)
	repl0 d = if p==0
	          then replaceMGPart d ps new
		  else err
        repl drawing =
	  case drawing of
	    LeafM _ _            -> err
	    MarkM     gctx    d  -> MarkM gctx (repl0 d)
	    SpacedM   spacer  d  -> SpacedM spacer (repl0 d)
	    PlacedM   placer  d  -> PlacedM placer (repl0 d)
	    ComposedM         ds ->
	      let pre = take (p-1) ds
                  d:post = drop ((p-1)::Int) ds
              in ComposedM (pre++replaceMGPart d ps new:post)

parentGctx gctx mg path =
  case path of
    [] -> gctx
    0:ps ->
      case mg of
        MarkM gctx' mg' -> parentGctx gctx' mg' ps
	SpacedM _   mg' -> parentGctx gctx mg' ps
	PlacedM _   mg' -> parentGctx gctx mg' ps
	_ -> ctrace "badpath" path gctx -- This is actually an error
    p:ps ->
      case mg of
	ComposedM   mgs ->
	  --{-
	  if p>length mgs
	  then ctrace "badpath" path gctx -- This is actually an error
	  else --}
	  parentGctx gctx (mgs!!(p-1)) ps
	_ -> ctrace "badpath" path gctx -- This is actually an error
