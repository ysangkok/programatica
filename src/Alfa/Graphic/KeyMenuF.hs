module KeyMenuF(keyMenuF) where
import AllFudgets
import Array

keyMenuF title alts =
  val>^=<oldMenuF menuFont title altkeys gfx
  where
    altAssoc = zip [1..] alts
    n = length alts
    a = array (1,n) altAssoc
    val i = case a ! i of (v,_,_,_) -> v
    key i = case a ! i of (_,k,_,_) -> [([Mod1],k){-,([Control],k)-}]
    gfx i = case a ! i of (_,_,c,s) -> hboxD' 15 [g s,keyD c]
      where
	keyD c = spacedD (hAlignS aRight) $
	         hboxD' 0 [diamondD,spacedD alignKeysS $ g c]
	diamondD = spacedD vCenterS $ g $ FlexD 10 False False d
	d (Rect p (Point w h)) = [FillPolygon Convex CoordModeOrigin
			  [p+pP w2 0,p+pP w h2,p+pP w2 h,p+pP 0 h2]]
	  where w2 = w `div` 2
	        h2 = h `div` 2
    
    altkeys = map (pairwith key) [1..n]

alignKeysS = minSizeS (pP 12 10) `compS` hCenterS
  -- This is a hack to make the keybord shortcuts align nicely in a column.
  -- It assumes that no character is wider than 12 pixels.

--ctrlkey = argFlag "menuctrlkey" False
