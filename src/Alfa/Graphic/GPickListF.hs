module GPickListF where
import Fud
import Fudgets

--gPickListF :: Graphic g => [(a,g)] -> F [(a,g)] (InputMsg (Int,a))
gPickListF placer alts =
    loopThroughRightF (absF (pickSP alts)) altListF
  where
    altListF = oldScrollF True (Point 200 260,Point 480 390) gfxF

    gfxF = showCommandF "dgpicklist" $
	   hyperGraphicsF' custom  (draw alts)

    custom = setAdjustSize False . setBorderWidth 0

    draw = padD 2.placedD placer. boxD . zipWith labelD [1..] . map snd

    pickSP alts = getSP $ either fromGfx fromOutside
      where
        fromOutside alts' =
	    putSP (changeGfx (draw alts')) $
	    pickSP alts'

        fromGfx p =
	    let i=p-1
	        alt = fst(alts!!i)
	    in putSP (Right (i,alt)) $
	       same

	same = pickSP alts
	changeGfx = Left. Left
