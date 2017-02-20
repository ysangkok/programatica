module SpaceF where
import AllFudgets

spaceF space f =
  let startcmds =  [layoutRequestCmd (plainLayout space True True),
                   XCmd $ ChangeWindowAttributes [CWEventMask eventmask]]
      eventmask = [KeyPressMask,KeyReleaseMask,
                   ButtonPressMask,ButtonReleaseMask]

      keysK = getK $ \ msg ->
                case msg of
		  Low (LEvt (LayoutSize _)) -> keysK
		  Low  (XEvt e)  -> putK (High (Left (Left e))) keysK
		  Low  e -> keysK
		  High (Right x) -> putK (High (Left (Right x))) keysK
		  High (Left y) -> putK (High (Right y)) keysK
      spaceK = changeBg "black" keysK
  in loopCompThroughRightF (groupF startcmds spaceK (f space))
