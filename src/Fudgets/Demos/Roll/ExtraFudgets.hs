module ExtraFudgets(module ExtraFudgets,EventMask(..),XEvent(..)) where
import AllFudgets

-- Put a fudget on top of a background

graphicsBgF bg d fud =
    filterRightSP >^^=< graphicsGroupF' custom fud >=^< route
  where
    custom = setBgColor bg . setSizing Static . setAdjustSize False .
	     setInitDisp d . setGfxEventMask []
    --route :: a -> Either (GfxCommand Void) a -- Grr. Resolves the overloading.
    route = Right

--instance Graphic Void


-- A new general combinator for reading events.

oeventF em fud =  eventF em (idLeftF fud)

eventF eventmask = serCompLeftToRightF . groupF startcmds eventK
  where
    startcmds = [XCmd $ ChangeWindowAttributes [CWEventMask eventmask],
	         XCmd $ ConfigureWindow [CWBorderWidth 0]]
    eventK = K $ mapFilterSP route
      where route = message low high
            low (XEvt event) = Just (High (Left event))
	    low _ = Nothing
	    high h = Just (High (Right h))


-- Movable shaped objects

shapedObjectF color shape rect0 sp =
    swindowF startcmds (Just rect0) $
    shapeK (const shape) $
    changeBg color $
    K $
    move `postMapSP` sp `serCompSP` mapFilterSP stripHigh
  where
    move = either (Low . XCmd . moveWindow) High
    startcmds = [XCmd $ MapRaised,
		 XCmd $ ChangeWindowAttributes [CWSaveUnder True]]
