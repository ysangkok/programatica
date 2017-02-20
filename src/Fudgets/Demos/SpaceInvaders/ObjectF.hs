module ObjectF where
import AllFudgets

objectF pos size bg fg evs gcattrs k =
    swindowF startcmds (Just (Rect pos size)) objectK
  where
    startcmds = map XCmd [ChangeWindowAttributes [CWEventMask eventmask],
                 ConfigureWindow [CWBorderWidth 0],
		 MapRaised]
    eventmask = if bg==fg
                then evs
                else ExposureMask:evs

    objectK =
      changeGetBackPixel bg $ \bg ->
      convGCattrsK (GCForeground fg:gcattrs) $ \gcattrs ->
      wCreateGC rootGC (GCBackground bg:gcattrs) $
      k pos

timerObjectF pos size bg fg evs gcattrs k =
    loopThroughRightF (objectF pos size bg fg evs gcattrs k) timerF
  
removeTimer = High $ Left $ Nothing
setTimer i f = High $ Left $ Just (i,f)
