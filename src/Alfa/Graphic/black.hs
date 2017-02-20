
main =
  let llcgs = zip' mgs rs
      lls = map fst llcgs
      cgs = map snd llcgs
      (ll',placer2) = overlayP lls
      rs = placer2 r
  in print (ll',(r,cgs))


mgs = [Layout (Point x x) True True | x <-[10,30..100]]
r = Rect (Point 0 0) (Point 100 100)

data Point = Point Int Int deriving Text
type Size = Point
data Rect = Rect Point Size deriving Text

data LayoutRequest = Layout Point Bool Bool deriving Text
--minsize (Layout s _ _) = s
--fixedh (Layout _ fh _) = fh
--fixedv (Layout _ _ fv) = fv

type Placer = [LayoutRequest] -> Placer2
type Placer2 = (LayoutRequest, Rect -> [Rect])

overlayP :: Placer
overlayP ls =
  let (ss,fhs,fvs) = unzip3 [(s,fh,fv) | (Layout s fh fv) <- ls]
      --ss = map minsize ls
      --fhs = map fixedh ls
      --fvs = map fixedv ls
      req = Layout (pMax ss) (or fhs) (or fvs)
      placer2 r = [ r | _ <- ls]
  in (req,placer2)

pmax (Point a b) (Point c d) = Point (max a c) (max b d)
pMax = foldr1 pmax

zip' [] _ = []
zip' (x:xs) ~(y:ys) = (x,y):zip' xs ys
