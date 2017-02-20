module MenuBar(
  menuBarF,staticMenuBarF,MenuBar(..),MenuEntries(..),MenuEntry(..)
) where
import AllFudgets
--import ListMap(lookpWithDefault)

data MenuBar menuCommand = MenuBar [(String, MenuEntries menuCommand)]
type MenuEntries menuCommand = [MenuEntry menuCommand]

data MenuEntry menuCommand =
      SubMenu String menuCommand -- (MenuEntries menuCommand)
    | MenuItem String menuCommand
    | MenuLabel String menuCommand
    | MenuSeparator menuCommand

data MenuMessage = Inactive | Active String

mbuttonMachine1 =
    let mbm m = m
    in  mbm

modstate = []

mousebutton = Button 1

menuSeparatorF optrect =
    let mask =
            [ExposureMask]
        startcmds =
            [ChangeWindowAttributes [CWEventMask mask, CWBackingStore Always], ConfigureWindow [CWBorderWidth 0]]
        optsize = mapMaybe rectsize optrect
        K sepDispKSP = changeBg "black" separatorDisplayK
    in  swindowF startcmds
                optrect
                (K $ preMapSP sepDispKSP mbuttonMachine1) 

separatorDisplayK =
    let dummy_size = Point 1 1
    in putsK [Low (layoutRequestCmd (plainLayout dummy_size True True))] $ nullK

mbuttonMachine2 =
    let mbm (Low (ButtonEvent _ _ _ _ Released _)) = High BMClick
        mbm (Low (EnterNotify _ _ _ _ _)) = High BMInverted
        mbm (Low (LeaveNotify _ _ _ _ _)) = High BMNormal
        mbm m = m
    in  mbm

subMenuButtonF optrect fname keys text =
    let mask =
            [EnterWindowMask, LeaveWindowMask, ButtonPressMask, ButtonReleaseMask,
             ExposureMask]
        transinit =
            if null keys then
                []
            else
                let tobutton (KeyEvent t p1 p2 s pressed _ ks _) | (s, ks) `elem`
                                                                 keys =
                        Just (ButtonEvent t p1 p2 modstate pressed mousebutton)
                    tobutton _ = Nothing
                in  [TranslateEvent tobutton [KeyPressMask, KeyReleaseMask]]
        startcmds =
            transinit ++
            [ChangeWindowAttributes [CWEventMask mask, CWBackingStore Always], ConfigureWindow [CWBorderWidth 0]]
        optsize = mapMaybe rectsize optrect
 	K subMenuBDispKSP = subMenuButtonDisplayK optsize fname text
    in  swindowF startcmds
                optrect
                (K $ preMapSP subMenuBDispKSP mbuttonMachine2)

subMenuButtonDisplayK opsize fname text =
    safeLoadQueryFont fname $ \fs ->
    wCreateGC rootGC [GCFunction GXcopy, GCFont (font_id fs)] $ \drawGC ->
    wCreateGC rootGC invertGCattrs $ \invertGC ->
    let Rect spos ssize = string_rect fs text
        margin = Point 3 1
	size =case opsize of
	        Just s -> s
		Nothing -> padd ssize (padd margin margin)
        invertitif b size' = if b then [Low (wFillRectangle invertGC (Rect origin size'))]
			     else []
	drawit state size' =
		let textpos = psub margin spos
		in  [Low ClearWindow,
		     Low (wDrawImageString drawGC textpos text)] ++
		    invertitif (state == BMInverted) size'
        buttonproc bstate size' =
	  let same = buttonproc bstate size'
              cont b = buttonproc b size'
	      redraw b s = putsK (drawit b s) (buttonproc b s)
	  in getK $ \bmsg ->
	     case bmsg of
	       Low (Expose _ 0) -> redraw bstate size'
	       Low (LayoutSize size'') -> redraw bstate size''
	       High BMClick -> putsK (invertitif (bstate == BMInverted) size' ++ [High Click]) (cont BMNormal)
	       High newstate -> putsK (invertitif (bstate /= newstate) size') (cont newstate)
	       _ -> same
    in putsK [Low (layoutRequestCmd (plainLayout size True True))] $
       buttonproc BMNormal size

labelDisplayK opsize fname text =
    safeLoadQueryFont fname $ \fs ->
    wCreateGC rootGC [GCFunction GXcopy, GCFont (font_id fs)] $ \labDrawGC ->
    let Rect spos ssize = string_rect fs text
        margin = Point 3 1
	size =case opsize of
	        Just s -> s
		Nothing -> padd ssize (padd margin margin)
        invertitif b size' = if b then [Low (wFillRectangle labDrawGC (Rect origin size'))]
			     else []
	drawit state size' =
		let textpos = psub margin spos
		in  [Low ClearWindow,
		     Low (wDrawImageString labDrawGC textpos text)] ++
		    invertitif (state == BMInverted) size'
        buttonproc bstate size' =
	  let same = buttonproc bstate size'
              cont b = buttonproc b size'
	      redraw b s = putsK (drawit b s) (buttonproc b s)
	  in getK $ \bmsg ->
	     case bmsg of
	       Low (Expose _ 0) -> redraw bstate size'
	       Low (LayoutSize size'') -> redraw bstate size''
	       High BMClick -> putsK (invertitif (bstate == BMInverted) size' ++ [High Click]) (cont BMNormal)
	       High newstate -> putsK (invertitif (bstate /= newstate) size') (cont newstate)
	       _ -> same
    in putsK [Low (layoutRequestCmd (plainLayout size True True))] $
       buttonproc BMNormal size

menuLabelF' optrect fname keys text =
    let mask =
            [EnterWindowMask, LeaveWindowMask, ButtonPressMask, ButtonReleaseMask,
             ExposureMask]
        transinit =
            if null keys then
                []
            else
                let tobutton (KeyEvent t p1 p2 s pressed _ ks _) | (s, ks) `elem`
                                                                 keys =
                        Just (ButtonEvent t p1 p2 modstate pressed mousebutton)

                    tobutton _ = Nothing
                in  [TranslateEvent tobutton [KeyPressMask, KeyReleaseMask]]
        startcmds =
            transinit ++
            [ChangeWindowAttributes [CWEventMask mask, CWBackingStore Always], ConfigureWindow [CWBorderWidth 0]]
        optsize = mapMaybe rectsize optrect
 	K lDispKSP = labelDisplayK optsize fname text
    in  swindowF startcmds
                optrect
                (K $ preMapSP lDispKSP mbuttonMachine1)

menuEntriesF :: (Eq a) => FontName -> [MenuEntry a] -> 
                F (Either b (Either PopupMenu (a, BMevents))) (Either b a)
menuEntriesF fname mes =
    idF >+< (fst >^=< menuPopupF (listLF (verticalP' 0) (map altButton mes)))
    where altButton (MenuItem lbl c) = (c, menuButtonF fname lbl>=^^<nullSP)
          altButton (MenuSeparator c) = (c, menuSeparatorF Nothing)
          altButton (MenuLabel lbl c) = (c, menuLabelF' Nothing fname [] lbl)
          altButton (SubMenu lbl c) = (c, subMenuButtonF Nothing fname [] lbl)

menuBarF :: (Eq a) => FontName -> MenuBar a -> F (MenuBar a) a 
menuBarF fname menuBar = dynF (staticMenuBarF fname menuBar) >=^< Left . staticMenuBarF fname

staticMenuBarF :: (Eq a) => FontName -> MenuBar a -> F b a
staticMenuBarF fname (MenuBar namemes) =
    loopLeftF (post >^=< (menuBar >=^^< concmapSP pre))
    where menuBar = listLF (horizontalP' 0) 
                            [(name, menuAlts mes >==< 
                                    menuButton (" " ++ name ++ " ")) | 
                             (name, mes) <- namemes]
 	  menuAlts = menuEntriesF fname
          menuButton = clickF1 Nothing fname
 	  post (from, Left output) = Left output 
 	  post (from, Right msg) = Right msg
 	  pre (Left active) = [(name, active) | (name, _) <- namemes]
 	  pre (Right msg) = []

clickF1 :: Maybe Rect -> FontName -> [Char] -> 
          F MenuMessage (Either MenuMessage (Either PopupMenu a)) 
clickF1 optrect fname name = 
    swindowF startcmds optrect (clickDisplayK1 optsize fname name)
    where optsize = mapMaybe rectsize optrect
          wattrs = [CWEventMask [ExposureMask, ButtonPressMask, 
	                         ButtonReleaseMask, OwnerGrabButtonMask, 
				 LeaveWindowMask, EnterWindowMask]]
	  startcmds = [ChangeWindowAttributes wattrs]

clickDisplayK1 :: Maybe Point -> FontName -> [Char] -> 
                 K MenuMessage (Either MenuMessage (Either PopupMenu a))
clickDisplayK1 optsize fname myname =
    safeLoadQueryFont fname $ \fs ->
    wCreateGC rootGC [GCFunction GXcopy, GCFont (font_id fs)] $ \drawGC ->
    wCreateGC rootGC invertGCattrs $ \invertGC ->
    let Rect spos ssize = string_rect fs myname
	strsize = rectsize . string_rect fs
	margin = Point 3 1
	size = case optsize of
		   Just s -> s
		   Nothing   -> padd ssize (padd margin margin)
	invertit size = [Low (wFillRectangle invertGC (Rect origin size))] 
	invertitif b size =
	    if b then invertit size else []
	drawit = drawname myname
	drawname name hi size =
	    [Low ClearWindow, Low (wDrawImageString drawGC textpos name)] ++
	    invertitif hi size
	    where textpos = psub (scalePoint 0.5 (psub size (strsize name))) spos
	buttonproc highlighted mode size = 
	    getK $ \bmsg ->
	    case bmsg of
                Low b@(ButtonEvent _ winpos rootpos [] Pressed (Button 1)) -> 
		    putsK (topopup (popupat rootpos winpos b) ++
		          tomenus (Active myname)) $
		    newmode (Active myname)
                Low (ButtonEvent _ _ _ _ Released (Button 1)) -> 
		    putsK (topopup PopdownMenu ++ tomenus Inactive) $ same
		Low (LeaveNotify _ _ _ _ NotifyUngrab) -> 
		    putsK (tomenus Inactive) $ same
		Low (LeaveNotify _ _ _ _ _) -> 
		    putsK (invertitif highlighted size) (cont False)
		Low b@(EnterNotify _ winpos rootpos _ _) -> 		
		    putsK (invertitif (not highlighted) size ++
		          case mode of
			      Inactive -> []
			      Active active ->
			          if active == myname then
				      []
				  else
				      tomenus (Active myname) ++
				      topopup (popupat rootpos winpos b)) $
		    buttonproc True (Active myname) size
		Low (Expose _ 0) -> redraw highlighted size
		Low (LayoutSize size') -> redraw highlighted size'
		High a@(Active newactive) ->
		    case mode of
		        Inactive -> newmode a
			Active active ->
			    if newactive == myname then
			        same
			    else
			       if active == myname then
			           putsK (topopup PopdownMenu) $ newmode a
			       else
			          newmode a
		High Inactive ->
		    case mode of
		        Inactive -> same
		        Active active ->
			    if active == myname then
			        putsK (topopup PopdownMenu) $ newmode Inactive
			    else
			        newmode Inactive
		_ -> same
	    where popupat r w = 
		      PopupMenu (padd (psub r w) (Point (-1) (ycoord size)))
		  topopup = (:[]) . High . Right . Left
		  tomenus = (:[]) . High . Left
	          same = buttonproc highlighted mode size
	          cont b = buttonproc b mode size
		  newmode m = buttonproc highlighted m size
	          redraw b s = putsK (drawit b s) $ buttonproc b mode s
    in putsK [Low (layoutRequestCmd (plainLayout size True True))] $
       buttonproc False Inactive size


