module BulletF(bulletF) where
import AllFudgets hiding (size)
import ObjectF

bulletF =
   timerObjectF hiddenpos size "white" "white" eventmask [] $ \pos gc->
   idleBulletK
  where
    eventmask = [VisibilityChangeMask]

idleBulletK =
  getK $ \msg ->
    case msg of
      High (Right p) -> let p' =p `psub` Point 0 height
		      in putsK [ Low $ XCmd $ moveWindow p'
                               , Low $ XCmd $ LowerWindow
                               , setTimer dt dt] $
			 bulletK p'
      _ -> idleBulletK

bulletK pos =
  let cont = bulletK pos
  in getK $ \msg ->
       case msg of
         High (Right p) -> cont
	 High (Left talarm) ->
	   let p'=pos `psub` Point 0 dy
	   in putK (Low $ XCmd (moveWindow p')) (bulletK p')
	 Low (XEvt (VisibilityNotify v)) ->
	   case v of
	     VisibilityUnobscured -> cont
	     _ ->
	       let cleanup = putK removeTimer .
	                     putK (Low $ XCmd $ moveWindow hiddenpos)
                   msgs = if ycoord pos<0
	                  then cleanup
			  else putK (High $ Right pos) . cleanup
			  --else Low (DoIO (AppendChan stdout (showP pos))):High pos:cleanup
	       in msgs idleBulletK
	 _ -> cont

size=Point width height
hiddenpos=Point (-width) 0
width=1
height=16
dt=30
dy=height

showP (Point x y) = "Hit "++show (x,y)
