module Barlayouter(barP, rightBelowP) where
import Geometry(Line(..), Point(..), Rect(..), Size(..), pP, padd)
import LayoutDir(LayoutDir(..), Orientation(..))
import LayoutRequest

barP :: Orientation -> Orientation -> Int -> Placer
barP ho vo sep
     [Layout (Point cw ch) fch fcv rpsc,
      Layout (Point rw rh) frh frv rpsr,
      Layout (Point bw bh) fbh fbv rpsb] =
    let reqw = cw `max` bw + sep + rw
        reqh = ch `max` rh + sep + bh
	rps = rpsc++rpsr++rpsb
    in  (Layout (pP reqw reqh) (fch && frh || fbh) (fcv && fbv || frv) rps,
         \(Rect p0@(Point x0 y0) (Point gotw goth)) ->
         let (gcw, grw, mx, rx) = splitspace reqw gotw cw rw sep ho
             (gch, gbh, my, by) = splitspace reqh goth ch bh sep vo
             r0 = pP (x0 + rx) (y0 + my)
             b0 = pP (x0 + mx) (y0 + by)
         in  [Rect (padd p0 (pP mx my)) (pP gcw gch), Rect r0 (pP grw gch), Rect b0 (pP gcw gbh)])

splitspace req got main bar sep or' =
    let flip' = or' == LeftOf || or' == Above
    in  if got < bar + sep then
            let mid = got `quot` 2
                (barp, mainp) = if flip' then (0, mid) else (mid, 0)
            in  (got - mid, mid, mainp, barp)
        else
            let gmain = got - bar - sep
                (barp, mainp) =
                    if flip' then (0, bar + sep) else (gmain + sep, 0)
            in  (gmain, bar, mainp, barp)

rightBelowP :: Int -> Placer
rightBelowP = barP RightOf Below

