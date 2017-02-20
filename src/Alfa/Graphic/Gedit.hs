module Gedit
  (gEditF,GEditOutput(..),GEditInput(..),GEditInputMode(..),inputMode0,Annot,Mod(..),Mod2(..),EditFunc,EdM,ADrawing(..),build,stdSelect,parentPath)
where
import AllFudgets
import KeyboardMenuF
import HO(apSnd,apFst)
import List(sort)
import ListUtil(mapFst)
import Subsequences(isPrefixOf) -- List(isPrefixOf) is buggy!!
--import Scans(mapAccumL)
import Annot
import EditFunc
import EditAnnot
import EditMonad
import UndoStack2
import ContribFudgets(completionStringF',stdcc,completeFromList,vSplitF')
import GeditOptions

-- For debugging:
--import Trace(trace)

#ifdef __HASKELL98__
#define map fmap
#endif

data GEditOutput state syntax editmsg
  = GEditChange (state,syntax) -- include new cursor !!
  | GEditSelect (DPath,syntax)
  | GEditEdit (state,syntax) (DPath,editmsg)
  | GEditMessage String
  | GEditMenuWindowOff

data GEditInput annot state syntax e
  = GEditNew (state,syntax)
  | GEditDo (DPath,(state,Maybe annot,syntax)->EditFunc annot state syntax e)
  | GEditUndo
  | GEditRedo
  | GEditChangeGlobalState (state->state)
	 -- for changes that should not be pushed on the undo stack
  | GEditZoomIn
  | GEditZoomOut
  | GEditMenuWindow Bool
  | GEditInputMode GEditInputMode
  | GEditMenuMode GEditMenuMode
  | GEditAutoScroll Bool
  | GEditAutoNextGoal Bool

data Modes
  = Mode { inputMode :: GEditInputMode,
           menuMode  :: GEditMenuMode,
	   autoScroll, autoNextGoal :: Bool
         }

mode0 = Mode inputMode0 menuMode0 autoscroll0 autonextgoal0

{-
gEditF :: (Graphic descr,Graphic g) =>
          (syntax -> ADrawing state syntax g) ->
	  (state -> syntax -> (descr,Menu state syntax g)) ->
	  --(ADrawing state syntax g -> DPath -> DPath) ->
	  syntax ->
	  --Menu state syntax g ->
	  state ->
	  F (DPath, Mod2 state syntax g) (GEditOutput state syntax)
-}
gEditF infoDispF draw drawTop describe button2func optEditDelete init state0 =
    placerF verticalP $
    loopThroughRightF (absF (editSP0 state0))
		      (filterLeftSP>^^=<vSplitF' aBottom sgfxF dispF)
  where
    defaultmenu = []   -- used to be an argument

    sgfxF = keyboardMenuF ((True,showStatus mode0),defaultmenu) $
            oldScrollF True (pP 500 300,pP 800 600) $
	    marginF 5 $
	    graphicsGroupF' custom stringInputPopupF
	where
	  custom = setInitDisp (drawTop state0 init) .
		   {-setAdjustSize False .-} setBorderWidth 0 .
		   setCursorSolid cursorsolid

    dispF =
        noStretchF False False $ vBoxF $
	infoDispF istate0 >=^< apFst ns >+< if pathdisp then displayF else nullF
      where initD = ns (fst (describe state0 Nothing [init] [])) -- ancestors?
            istate0 = (initD,(draw state0,drawTop state0))
            ns = spacedD (noStretchS True True)


    toGfx   = Left. Left. Right. Left
    fromGfx = left. id  . right. left        -- id because of filterLeftSP above
    toStringInputPopup   = Left. Left. Right. Right
    getGfxPlaces = toGfx.GetGfxPlaces
    toDisp = Left. Right. Right
    toGDisp = Left. Right. Left
    toMenu = Left. Left. Left
    menuToMenu = toMenu. Right. Right
    keyToMenu = toMenu. Right. Left
    popToMenu = toMenu. Left
    out = Right

    showGfxSP = showGfxSP' False

    showGfxSP' center zoompath path =
	if zoompath `isPrefixOf` path
	then putSP (showGfx (dropl zoompath path))
	else id
      where
	showGfx path = toGfx (ShowGfx path (Nothing,optalign))
	optalign = if center then Just alignment else Nothing
	alignment = argReadKey "valign" 0.5

    changeGfxSP zoompath changes =
       if null changes'
       then id
       else putSP (changeGfx changes')
      where
	changeGfx = toGfx.ChangeGfx
	changes' = concatMap adj changes
	adj (path,ch@(cursorOn,optChange)) =
	    if zoompath `isPrefixOf` path
	    then [(dropl zoompath path,ch)]
	    else if path `isPrefixOf` zoompath
		 then [([],(cursorOn,
			   optChange >>=
			   flip maybeDrawingPart (dropl path zoompath)))]
		 else []


    redraw drawing =
      case drawing of
        LabelD (Annot a _) _ -> flip draw a
	_ -> drawTop -- !!

    modop d = runEd.modop' d
    --modop' :: Mod2 s a e -> Mod s (ADrawing s a e)
    modop' (Left  dmod) = dmod
    modop' (Right amod) =
       \ drawing -> mapStateEd (redraw drawing) (amod (build drawing))

    undoStk0 = undoStack (Just undoHistory)
    undoHistory = argReadKey "undo_history" 10

    editSP0 state0 =
      --changeGfxSP [] [([],(True,Nothing))] $
      editSP mode0 undoStk0 state0 (drawTop state0 init) [] []

    editSP mode undoStk state drawing zoompath cursor =
	  --putSP (toDisp (show (cursor,sibls,children))) $
	  getSP $ either (either fromMenu gfxGroupInputSP) inputSP
      where
	same = editSP mode undoStk state drawing zoompath cursor
	editDelete = maybe same (\f->replaceSP (f thisAbs)) optEditDelete
	gfxGroupInputSP = either gfxInputSP stringPopupInputSP
	stringPopupInputSP = either newMenuSP (either spaceKeySP menuPickSP)
	  where newMenuSP m =
		  putSP (menuToMenu ((False,showStatus mode),m)) $
		  same
        	spaceKeySP _ = keySP [] "space" " " -- hmm

        fromMenu = either popMenuSP menuPickSP
	popMenuSP _ = putSP (out GEditMenuWindowOff) same
        menuPickSP = either (flip putErrMsg same) replaceSP

        centerGfxSP = showGfxSP' (autoScroll mode)

	gfxInputSP msg =
	  case msg of
	    GfxKeyEvent { gfxState=gfxState, gfxKeySym=gfxKeySym, gfxKeyLookup=gfxKeyLookup } ->
	      keySP gfxState gfxKeySym gfxKeyLookup
	    GfxButtonEvent { gfxState=gfxState, gfxButton=Button 2, gfxType=Pressed, gfxPaths=gfxPaths } ->
	      click2SP gfxState gfxPaths
	    GfxButtonEvent { gfxState=gfxState, gfxButton=Button 1, gfxPaths=gfxPaths } ->
	      if Control `elem` gfxState then click2SP gfxState gfxPaths else
	      mouseSelectSP gfxPaths
	    GfxMotionEvent { gfxState=gfxState, gfxPaths=gfxPaths } | Button1 `elem` gfxState ->
	      mouseSelectSP gfxPaths
	    _ -> same

	click2SP mods pathmsg =
	  case pathmsg of
	     ((path,_):_) ->
		 replaceSP (button2func (Shift `elem` mods) (state,annot,thisAbs) clickedAbs)
	       where
	         annot = case this of
			   LabelD (Annot d _) _ -> Just d
		           _ -> Nothing
		 clickedAbs =
		    build (drawingPart' drawing (stdSelect drawing (zoompath++path)))
	     _ -> same

        keySP mods sym ascii =
	    case sym of
	      "Left" | meta -> ifPresent sibls $ kbSelect left_path
	             | True -> kbSelect prev_leaf
	      "Right"| meta -> ifPresent sibls $ kbSelect right_path
	             | True -> kbSelect next_leaf
	      "Up"     -> ifPresent cursor   $ kbSelect up_path
	      "Down"   -> ifPresent children $ kbSelect down_path
	      "Delete" -> editDelete
	      "BackSpace" -> editDelete
	      "Return" -> nextPlaceholder
	      "space" -> nextPlaceholder
	      _ -> case ascii of
	             [c] -> putSP (keyToMenu c) same
		     _   -> same
	  where
	    meta = Mod1 `elem` mods
            ifPresent path sp = if null path then same else sp
	    nextPlaceholder =
		  case next drawing cursor of
	            [] -> same -- !!!
		    cursor' -> kbSelect cursor'
		where next = if Shift `elem` mods
		              then prevHole
			      else nextHole
	    kbSelect = selectSP'
	    down_path = cursor++head children
	    up_path = parentPath drawing cursor
	    children = map fst (selectableChildren this)
	    --parent  = drawingPart' drawing up_path
  
	    next_leaf = moveRight drawing cursor
	    prev_leaf = moveLeft  drawing cursor

	    left_path = last sibls
	    right_path = head sibls
	    sibls = right_siblings++left_siblings
	      where (left_siblings,right_siblings) = siblings drawing cursor

        this = drawingPart' drawing cursor
	thisAbs = build this

	inputSP gEditInput =
	    case gEditInput of
	      GEditNew (state',syntax') ->
	        new (state',drawTop state' syntax',[])
	      GEditDo (path,editFunc) ->
		--changeGfxSP zoompath [(cursor,(False,Nothing))] $
		replaceSP' path (editFunc (state,annot,thisAbs))
	       where
	         annot = case this of
			   LabelD (Annot d _) _ -> Just d
		           _ -> Nothing
	      GEditUndo -> case undo undoStk of
	                     Just (eState,undoStk') -> change eState undoStk'
			     _ -> same -- !! error msg
	      GEditRedo -> case redo undoStk of
	                     Just (eState,undoStk') -> change eState undoStk'
			     _ -> same -- !! error msg
	      GEditChangeGlobalState upd -> change eState' undoStk'
		where eState' = upd3 (state,drawing,cursor)
		      undoStk' =
		          map upd3 undoStk
			-- minor space leak if undo isn't used
			-- many state changes -> uneeded repeats of build/draw
		      upd3 (state,drawing,cursor) = (state',drawing',cursor')
			where
			  state' = upd state
			  drawing' = drawTop state' (build drawing)
			  cursor' = stdSelect drawing' cursor
	      GEditZoomIn -> zoom cursor
	      GEditZoomOut -> zoom []
	      GEditMenuWindow on -> putSP (popToMenu on) $ same
	      GEditInputMode imode' ->
	          (if imode'/=Completions then popdownStringInput else id) $
	          editNewSP'' mode' undoStk state True drawing zoompath cursor
	        where mode' = mode { inputMode=imode' }
	      GEditMenuMode mmode' ->
	          editNewSP'' mode' undoStk state True drawing zoompath cursor
	        where mode' = mode { menuMode=mmode' }
	      GEditAutoScroll ascroll ->
	          --editNewSP'' mode' undoStk state True drawing zoompath cursor
		  editSP mode' undoStk state drawing zoompath cursor
	        where mode' = mode { autoScroll=ascroll }
	      GEditAutoNextGoal anextgoal ->
		  editSP mode' undoStk state drawing zoompath cursor
	        where mode' = mode { autoNextGoal=anextgoal }
	  where
	    zoom zoompath' =
	      updGfx state drawing [] zoompath' cursor $ \ drawing'' ->
	      (if null zoompath' then centerGfxSP zoompath' cursor else id)$
	      editNewSP' undoStk state True drawing'' zoompath' cursor

	    new eState = doit undoStk0 eState $ change eState
	    change (state',drawing',cursor') undoStk' =
	      updGfx state' drawing' [] zoompath cursor' $ \ drawing'' ->
	      editNewSP' undoStk' state' True drawing'' zoompath cursor'
        mouseSelectSP pathmsg =
	  case pathmsg of
	    [] -> {-ctrace "k" "unselectable"-} same
	    ((path,_):_) ->
	      --ctrace "mouseselect" (show (zoompath++path,visiblePlaceHolderPaths drawing)) $ 
	      selectSP' (zoompath++path)
	selectSP' path =
	    let cursor' = stdSelect drawing path
	    in -- delete old cursor and draw the new cursor
	       if cursor'/=cursor
	       then changeGfxSP zoompath
		           [(cursor,(False,Nothing)),
			   (cursor',(True,Nothing))] $
		    showGfxSP zoompath cursor' $
	            putSP (out (GEditSelect (cursor',build (drawingPart' drawing cursor')))) $
	            editNewSP'' mode undoStk state True drawing zoompath cursor'
	       else same
{-
	       (if cursor'/=cursor
		then changeGfxSP zoompath
		       [(cursor,(False,Nothing)),
			(cursor',(True,Nothing))]
	       else id) $
	       putSP (out (GEditSelect (cursor',build (drawingPart' drawing cursor')))) $
	       editNewSP'' mode undoStk state isdone drawing zoompath cursor'
-}
	replaceSP = replaceSP' cursor
	replaceSP' path editFunc =
	  case editFunc of
	    EditLocal mod2 -> replaceLocalSP path mod2
	    EditParent mod2f ->
	      let parent_path = parentPath drawing path
	          rel_path = dropl parent_path path
	      in replaceLocalSP parent_path (mod2f rel_path)
	    EditPlaceHolders pmod ->
	      let part = build (drawingPart' drawing path)
	          phs = map (apSnd build) (placeHolders drawing)
	      in case runEd (pmod ((path,part):phs)) state of
	           Left err -> putErrMsg err same
	           Right (phs',state') ->
		     replaceManySP path state' phs'
            EditGlobal gmod ->
	      case runEd (gmod cursor drawing (build drawing)) state of
	        Left err -> putErrMsg err same
		Right (abschanges,state') ->
		  replaceManySP cursor state' abschanges
	    EditExternal emsg ->
	         putSP (out (GEditEdit (state,build drawing) (cursor,emsg))) $
		 same
            EditShowMessage msg -> displayMessage msg same
	    EditShowMenu menu -> displayMenu menu $ same
	    _ -> error "Unimplemented kind of EditFunc"

        replaceManySP path state' [] = editNewSP state' True drawing cursor
        replaceManySP path state' abschanges =
            let changed_paths = map fst abschanges
		drawing'' = foldr repl1 drawing abschanges
  		  where repl1 (path,abs) drawing = updatePart drawing path upd
			  where upd part = redraw part state' abs
	        (drawing',changes) = foldr repl1 (drawing'',[]) changed_paths
		  where
		    repl1 path dcs@(drawing,changes) =
			if any ((`isPrefixOf` path).fst) changes
			then dcs
			else (drawing',
			      (ppath,ppart):
			      filter (not.(ppath `isPrefixOf`).fst) changes)
		      where
			(drawing',ppath,ppart)= redrawPart state' drawing path
		cursor' = case findHole drawing' oldcursor' of
			    path'@(_:_) | autoNextGoal mode
					  && isPlaceHolder this -> path'
		            _ -> stdSelect drawing' path -- !! []
		  where
		    --oldcursor' = path -- old solution
		    -- Experiment: Improved cursor positioning after
		    -- instantianting a place holder whose parent is redrawn.
		    oldcursor' = changedParent path visibleChanges -- new solu
		    changedParent path [] = path
		    changedParent path ((cpath,_):changes) =
		      if cpath `isPrefixOf` path
		      then changedParent cpath changes
		      else changedParent path changes

		visibleChanges =
		  [(path,(False,Just d))|(path,d)<-changes,
		                         isVisibleDrawingPart drawing' path]
	    in --trace (show (map fst visibleChanges)) $
	       changeGfxSP zoompath visibleChanges $
	       changeGfxSP zoompath [(cursor',(True,Nothing))] $
	       (centerGfxSP zoompath $
		  if isPlaceHolder this && path `isPrefixOf` cursor'
		  then --trace ("childpath "++show path) $
	               path
		  else --trace ("cursorparent "++show (parentPath drawing' cursor')) $
		       parentPath drawing' cursor') $
	       editNewSP state' True drawing' cursor'
	replaceLocalSP path mod2 =
	  let part = drawingPart' drawing path
	  in case modop mod2 part state of
	       Left err -> putErrMsg err same
	       Right (part',state') ->
	         let drawing' = replacePart drawing path part'
		     holes = visiblePlaceHolderPaths part'
		     (cursor',holepath) =
		         case holes of
			   p:_ -> (path++p,p)
			   _   -> (path,[])
		 in updGfx state' drawing' path zoompath holepath $ \ drawing'' ->
		    editNewSP state' True drawing'' cursor'
	updGfx state drawing changed_path zoompath cursor cont =
	  let (drawing',redraw_path,redraw_part) =
		redrawPart state drawing changed_path
	  in changeGfxSP zoompath [(redraw_path,(False,Just redraw_part)),
	                       (changed_path++cursor,(True,Nothing))] $
	     cont drawing'
	editNewSP state' changeMenu drawing' cursor' =
	     doit undoStk (state',drawing',cursor') $ \ undoStk' ->
	     editNewSP't (undoStk', state', changeMenu, drawing', zoompath, cursor')
	-- Workaround "Out of A-regs" on i386. Grr!!
	editNewSP't (undoStk', state', changeMenu, drawing', zoompath, cursor') =
	  editNewSP' undoStk' state' changeMenu drawing' zoompath cursor'

	editNewSP' undoStk' state' changeMenu drawing' zoompath' cursor' =
	  putSP (out (GEditChange (state',build drawing'))) $
	  putSP (out (GEditSelect (cursor',build (drawingPart' drawing' cursor')))) $
	  editNewSP'' mode undoStk' state' changeMenu drawing' zoompath' cursor'
	editNewSP'' mode' undoStk' state' changeMenu drawing' zoompath' cursor' =
	  let part = drawingPart' drawing' cursor'
              (d,descr1,hole) = 
		 case part of
		   LabelD (Annot d _) _ -> (Just d,show d,isHole d)
		   _ -> (Nothing,"?",False)
	      (descr2,menu) = describe state' d (ancestors drawing' cursor') phsinfo
	      phsinfo = [(isVisibleDrawingPart part phpath,
			  build (drawingPart' ph phpath{-??looks buggy...-})) |
			 (phpath,ph)<-placeHolders part]
	  in putSP (toDisp (unwords [descr1,show cursor'])) $
             putSP (toGDisp (descr2,(draw state',drawTop state'))) $
	     (if changeMenu
	      then displayMenu' mode' zoompath' cursor' hole menu
	      else id) $
	     editSP mode' undoStk' state' drawing' zoompath' cursor'

        displayMenu = displayMenu' mode zoompath cursor False -- hmm

        displayMenu' mode' zoompath' cursor' hole (EditMenu menu) =
	    putSP (menuToMenu ((True,showStatus mode'),shownmenu)) .
	    popupStringInputSP (inputMode mode') zoompath' cursor' menu hole
	  where
	    shownmenu=
	      case inputMode mode' of
		Completions | hole -> []
		_ -> typecheckMenu (menuMode mode') menu

        putErrMsg err = displayMessage (Left err::(Either String String))
        displayMessage msg cont =
	    case msg of
	      Right info -> msgSP greenD info cont
	      Left err   -> putSP (toGfx $ BellGfx 0) $
			    msgSP redD err cont
	  where
	    greenD = fgD ["green4","black"]
	    redD = fgD ["Red","Black"]

	    msgSP colorD msg cont =
	        putSP (out $ GEditMessage msg) $
                putD (colorD msgD) cont
	      where
		msgD = fontD font $ vboxlD' 0 (map g ls)
		ls = lines msg
		font = if length ls> 1 then defaultFont else labelFont
                putD = putSP . toGDisp' . padD 2
	        toGDisp' d = toGDisp (d,(undefined,undefined))

	redrawPart state drawing path =
	    case redrawAncestor drawing path of
	      Nothing -> (drawing,path,drawingPart' drawing path)
	      Just redraw_path ->
		  (drawing',redraw_path,rpart')
		where
		  rpart       = drawingPart' drawing redraw_path
		  rpart'      = redraw rpart state (build rpart)
		  drawing'    = replacePart drawing redraw_path rpart'
	  where
            redrawAncestor drawing path =
	      --ctrace "redraw" ("redrawAncestor",path) $
	      case drawingPart' drawing path of
	        LabelD (Annot d _) _
		  | redrawFromParent d || not (okRedrawPoint d) ->
		  --ctrace "redraw" (d,redrawFromParent d,okRedrawPoint d) $
		  redrawAncestor' drawing (parentPath drawing path)
		dr -> --ctrace "redraw" (head $ words $ show dr) $
	              Nothing

            redrawAncestor' drawing [] = Just []
            redrawAncestor' drawing path =
	      --ctrace "redraw" "redrawAncestor'" $ -- "'"
	      case drawingPart' drawing path of
	        LabelD (Annot d _) _
		  | not (okRedrawPoint d) ->
		  redrawAncestor' drawing (parentPath drawing path)
		_ -> Just path

        popupStringInputSP imode zoompath cursor menu hole cont =
	    case imode of
	      Completions ->
	         if hole && zoompath `isPrefixOf` cursor
	         then cmdContSP (getGfxPlaces [dropl zoompath cursor])
			        (fromGfx answer) $ \ place ->
	              putSP (toStringInputPopup (Popup (rectpos place) menu)) $
		      cont
	         else popdownStringInput $
	              cont
	      _ -> cont
	  where
	    answer (GfxPlaces [place]) = Just place
	    answer _ = Nothing

        popdownStringInput = putSP (toStringInputPopup Popdown)

stringInputPopupF =
    delayF $
    mapstateSP select [] >^^=< popupGroupF (const 0,wattrs,nullK) popupF
  where
    popupF = idRightF csF  >=^^< concatMapSP distr
    csF = completionStringF' stdcc (setSizing Dynamic. setBgColor cursorcolor)

    wattrs = [CWEventMask [ButtonReleaseMask]] -- to avoid propagating them

    distr menu = [Right menu,Left (Right ""),
		  Left (Left (completeFromList $ sort $ map (fst.snd) menu))]

    select menu (Right menu') = (menu',[])
    select menu (Left (Left cs)) =
      (menu,[Left [item|item@(_,(s,_))<-menu,s `elem` completions]])
      where
        completions = [pre++compl|(pre,compl)<-cs]

    select menu (Left (Right inp)) =
      case inputDone inp of
        Just "" -> (menu,[Right $ Left ()])
        Just s -> (menu,take 1 [Right $ Right f | (f,(s',_))<-menu, s'==s])
	_ -> (menu,[])

isDone (InputDone _ _) = True
isDone _ = False

typecheckMenu TypecheckedMenu = filter (isRight.fst)
typecheckMenu UntypedMenu = id
typecheckMenu UntypedNoStatusMenu = id

showStatus mode =
  case menuMode mode of
    UntypedNoStatusMenu -> False
    _ -> True

moveLeft drawing [] = leftmostLeaf drawing
moveLeft drawing cursor =
  case siblings drawing cursor of
    (ss@(_:_),_) -> s++rightmostLeaf (drawingPart' drawing s)
      where s = last ss
    _ -> moveLeft drawing (parentPath drawing cursor)

rightmostLeaf drawing =
  case selectableChildren drawing of
    [] -> []
    cs -> case last cs of (p,d) -> p++rightmostLeaf d

moveRight drawing [] = rightmostLeaf drawing
moveRight drawing cursor =
  case siblings drawing cursor of
    (_,s:_) -> s++leftmostLeaf (drawingPart' drawing s)
    _ -> moveRight drawing (parentPath drawing cursor)

leftmostLeaf drawing =
  case selectableChildren drawing of
    [] -> []
    (p,d):_ -> p++leftmostLeaf d

siblings drawing cursor = apSnd (drop 1) (break (==cursor) cs)
  where
    cs = map ((parent_path++).fst) (selectableChildren parent)
    parent = drawingPart' drawing parent_path
    parent_path = parentPath drawing cursor

nextHole drawing cursor = findHole' drawing (>cursor)
findHole drawing cursor = findHole' drawing (>=cursor)

prevHole drawing cursor = findHole'' reverse drawing (<cursor)

findHole' x = findHole'' id x
findHole'' order drawing p =
  case break p (order (visiblePlaceHolderPaths drawing)) of
    (_,next:_)  -> next
    (first:_,_) -> first
    _           -> [] -- !!! hmm

--build d = ctrace "build" (show d) (build' d)

build d@(LabelD (Annot _ b) _) = b (map (build.snd) (annotChildren d))
build (SpacedD _ d) = --Because spacer nodes don't have unique paths (obsolete?)
  ctrace "build" "SpacedD" $
  build d
build d =
  case annotChildren d of
    [(_,d1)] -> build d1
    ds -> error ("build: "++show (length ds)++" annotated children")

parentPath drawing path = stdSelect drawing (up path)

ancestorPaths drawing [] = [[]]
ancestorPaths drawing path = path:ancestorPaths drawing (parentPath drawing path)

ancestors drawing = map (build . drawingPart' drawing) . ancestorPaths drawing

visiblePlaceHolderPaths drawing =
  filter (isVisibleDrawingPart drawing) (placeHolderPaths drawing)

placeHolderPaths d = map fst . placeHolders $ d

placeHolders d = annotChildren' p d
  where p (Annot s _) = isHole s

isPlaceHolder d =
  case d of
    LabelD (Annot s _) _ -> isHole s
    _ -> False

selectable (Annot s _) = isSelectable s

selectableChildren drawing =
  filter (isVisibleDrawingPart drawing.fst) (annotChildren' selectable drawing)

stdSelect drawing path =
  drawingAnnotPart' selectable drawing (visibleAncestor drawing path)

dropl = ltail

no _   = Nothing
yes    = Just
left f = either f no
right  = either no

cursorcolor    = argKeyList "cursor"      ["yellow"]
cursorsolid    = argFlag "cursorsolid"    False
pathdisp       = argFlag "pathdisp"       False
--completionmenu = argFlag "completionmenu" completions

-- debugging:

drawingPart' drawing path = drawingPart drawing path {-
  case maybeDrawingPart drawing path of
    Nothing -> error $ unwords ["drawingPart'",show drawing,show path]
    Just d -> d
--}
