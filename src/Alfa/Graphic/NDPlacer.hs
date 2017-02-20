module NDPlacer where
import AllFudgets

ndP = ndP' defaultSep

ndP' :: Distance -> Placer
ndP' sep ls{-@[prems,bar,name,concl]-} = (req,placer2)
  where
    req = Layout (Point w h) False False []
    w = maximum [conclw,barw,premsw] + sep + namew
    h = maximum [conclh+sep+barh+sep+premsh,nameh]
    ss{-@[premss,bars,names,concls]-} = map minsize ls
    [premsw,barw,namew,conclw] = map xcoord ss
    [premsh,barh,nameh,conclh] = map ycoord ss
    
    placer2 (Rect (Point x0 y0) (Point aw ah)) = [premsr,barr,namer,conclr]
      where
        rulew = aw-namew-sep
	bary = y0+ah-conclh-sep
	conclr = rR x0 (y0+ah-conclh) rulew conclh
	barr = rR x0 bary rulew barh
	namer = rR (x0+aw-namew) (bary-nameh `div` 2) namew nameh
	premsr = rR x0 (y0+ah-h) premsw premsh

