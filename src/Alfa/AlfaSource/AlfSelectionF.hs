module AlfSelectionF(alfSelectionF) where
import Fudgets

tr = ctrace "sel"

alfSelectionF = loopThroughRightF (absF ctrlSP0) selectionF
  where
    ctrlSP0 =
        -- Some other X client currently has the selection, so if we receive
	-- the PasteSel request it is passed on to selectionF.
        getSP $ either fromSelectionF fromAlf
      where
	same = ctrlSP0
	fromAlf selcmd =
	  case selcmd of
	    Sel selection -> selectSP selection
	    PasteSel -> putSP (Left PasteSel) same
	    _ -> same -- nothing to paste or clear

    fromSelectionF msg =
      case msg of
        SelNotify s -> tr ("Notify "++s) $
	               putSP (Right (SelNotify (Left s)))
		       ctrlSP0
	LostSel     -> tr "LostSel" ctrlSP0

    selectSP (stringRep,internalRep) =
      -- Here we become the selection owner.
      tr ("Sel "++stringRep) $
      putSP (Left (Sel stringRep)) $
      selectionOwnerSP internalRep

    selectionOwnerSP selection =
       -- We are now the selection owner, so if we receive
       -- the PasteSel request we can answer directly.
       getSP $ either fromSelectionF fromAlf
     where
       same = selectionOwnerSP selection
       fromAlf selcmd =
         case selcmd of
	   Sel selection -> selectSP selection
	   PasteSel   -> putSP (Right (SelNotify (Right selection))) $
	                 same
	   ClearSel   -> --put clear to selectionF $
	                 ctrlSP0


