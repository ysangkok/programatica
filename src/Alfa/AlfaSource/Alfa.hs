import Fudgets hiding (StreamProcIO)
import Fud
import DialogueIO hiding (IOError)
import ContribFudgets(titleShellF',delayedAuxShellF)
import Gedit
import GeditOptions(GEditInputMode(..))
import EditFunc(editNop,editError,editShowMessage)
import EditAnnot
import LoadModules
import TextFileConv
import AlfEditorOps
import AlfState
import AlfSyntax
import EvalPopupF(evalPopupF)
import LayoutEditorF(layoutEditPopupF)
import FileMenuF(unnamed,fileNamePopupF,libDir,popup)
import AlfaMenuBar
--import KeyMenuF
import qualified AlfBrowser as Br
import InfoDispF
import ParsedStringPopupF(parsedStringPopupOptF,parsedTextPopupOptF)
import AlfSelectionF
import FileConv(prSyntax)
import Fonts(loadFontsF)
import UAbstract(namesDecl,Var(..),Decl(..),Module(..))
import UAnnots() -- DispFuName
import qualified AlfModules as U
import qualified Variables as V
import qualified UPrintLatex as UPr
import qualified AlfaText()
import qualified AlfaPlugin()
import Now
import DrawOptions(declDetail0,DrawOptions(..))
import Maybe(fromMaybe)

main =
  do state0 <- initState [libDir]
     let prstate = state0 unnamed undefined
     case argKey "print" "" of
       "" -> case argKey "check" "" of
	       "yes" -> alfa_check prstate
               _ -> alfa_interactive state0
       "yes" -> alfa_print prstate declDetail0
       s -> case reads s of
	      (dispf,_):_ | dispf `elem` map fst (altdispfs prstate) ->
		alfa_print prstate (DeclAsTextBy dispf)
	      _ -> error $ "Alfa: bad print options: "++s

alfa_check state = fudlogue $ ioF $ foldr checkModuleK (exitK 0) args
  where
    checkModuleK filename cont =
      loadModuleK filename state $
      either error $ \ r ->
      case ok r of
        Nothing -> cont
	Just err -> echoK (filename++": "++err) (exitK 1)

    exitK n = hIOSucc (Exit n) nullK

    ok (ModuleS (Module ds),state) =
       if all correct ds
       then case length (metaVars state) of
	      0 -> Nothing
              n -> Just $ "Found "++show n++" metavariable(s)"
       else Just "Type error"

    correct (Decl c _) = c
    correct _ = True

    nometavars = null . metaVars

alfa_print state view = fudlogue $ ioF $ foldr prModuleK exitK args
  where
    prModuleK filename cont =
      loadModuleK filename state $
      either error $ \ (syntax',state') ->
      let dOpts = (drawOptions state'){declDetail=view}
	  contents = UPr.prTopSyntax (altdispfs state') dOpts syntax'
	  filename' = filename++".tex"
      in hIOSucc (WriteFile filename' contents) $
	 cont

    exitK = hIOSucc (Exit 0) nullK

alfa_interactive state0 =
   do putStrLn alfa_version
      fudlogue (loopLeftF $ titleShellF' pm "Alfa" (mainF state0))
  where pm = setSizing (argReadKey "sizing" Static)
        sizS = maybe idS sizeS defaultSize
	mainF state0 = spacer1F sizS (alfaF0 state0)


alfaF0 state0 = loadFontsF $ alfaF . state0 unnamed

alfaF state = vBoxF (Left>^=<editorLoopF)
  where
    editorLoopF =
       loopThroughRightF
         (ioF $ ioK0 state)
	 ((menubarF>+<alfEditF state>+<Br.alfBrowserF)>+<(popupsF>+<alfSelectionF))

    dOpts = drawOptions state

    alfEditF = alfEditF' (Just deleteSomething) empty

    alfEditF' = gEditF infoDispF drawAlfSyntax drawAlfTopSyntax describe (refineByClicking dOpts)

    popupsF = (parsedInputPopupsF >+< layoutEditPopupF)
	      >+< (fileNamePopupF >+< (evalPopupF state>+< gfxWindowF))

    parsedInputPopupsF =
      parsedTextPopupOptF  "Alfa Text Input (multiple lines)" >+<
      parsedStringPopupOptF "Alfa Text Input (one line)"

    menubarF = hBoxF (menusF >*< spacer1F (hvAlignS aRight aCenter) gcWarningF)
{-
    menusF = placerF (leftS `spacerP` horizontalP)
                     ({-putF NewFile-} fileMenuF>+<
		      (editMenuF>+<(putF Redraw viewMenuF>+<utilsMenuF)))
---}
    menusF = spacer1F leftS ({-putF (View Redraw)-} (alfaMenuBarF' plugprs plugpas))
    plugprs = pluginPrinters state
    plugpas = pluginParsers state

    --gfxWindowF :: Graphic g => F g nothing
    gfxWindowF = wrapF $ delayedAuxShellF "Graphics Display" (scrollF displayF)
      where
        wrapF fud = loopLeftF (fud>=^^<concatMapSP pre)
        pre (Right x) = [Left True,Right x]
	pre y = [y]

ioK0 state=
  startupK [filePopupTag ((Left Open,popup),filename) | filename<-take 1 args] $
  ioKempty state

ioKempty state =
  putTitle ("Alfa "++compileDate++": new file") $
  ioK ((setCurrentFile unnamed state,empty),([],empty))

ioK state@(gestate@(alfstate,file),cursor@(path,syntax)) =
    --echoK filename $
    putEvalState (alfstate,syntax) $
    getK $ message lowInput highInput
  where
    filename = currentFile alfstate
    lowInput _ = same
    dOpts = drawOptions alfstate

    -- BEGIN routing
    highInput = either fromLoop fromOutside
    fromLoop = either (either (fromMenubar `either` alfOutput) fromBrowser)
		      (fromPopup `either` selectionInput)
    fromPopup = (giveInput `either` layoutInput)
		`either`
		(filePopupOutput `either` (evalPopup `either` gfxWindow))
    fromOutside _ = same
    --fromMenubar = either fileCmd (either editCmd (either viewCmd utilCmd))
    fromMenubar menucmd =
      case menucmd of
	File filecmd     -> fileCmd filecmd
	Edit editcmd     -> editCmd editcmd
	View viewcmd     -> viewCmd viewcmd
	Options optioncmd -> optionCmd optioncmd
	Utils utilcmd    -> utilCmd utilcmd

    putAlf          = putK . High . Left . Left  . Left . Right
    putMenu         = putK . High .  Left . Left  . Left . Left
    putBrowser      = putK . High . Left . Left  . Right . Right
    putBrowserTitle = putK . High . Left . Left  . Right . Left
    putGiveInput    = putK . High . Left . Right . Left . Left . Left
    getGiveInput    =        high . left . right . left . left . left
    putEditLayout   = putK . High . Left . Right . Left . Left . Right
    getEditLayout   =        high . left . right . left . left . right
    putFilePopup    = putK . High . Left . Right . Left . Right . Left
    putEvalPopup    = putK . High . Left . Right . Left . Right . Right . Left
    putGfxWindow    = putK . High . Left . Right . Left . Right . Right . Right
    putSelection    = putK . High . Left . Right . Right
    getSelection    =        high . left . right . right
    -- END routing

    putEvalState    = putEvalPopup . Right
    putEvalWindow   = putEvalPopup . Left

    same = ioK state
    editfile filename' = gEditDo ([],changeState (setCurrentFile filename')) $
                         same
                         --putTitle filename' $
			 --ioK ((setCurrentFile filename' alfstate,file),cursor)
    editchange gestate'@(alfstate',_) =
	 --echoK "got state" $
	  (if filename'/=filename then putTitle filename' else id) $
          ioK (gestate',cursor) -- same cursor?!
        where filename' = currentFile alfstate'
    editselect cursor' = --echoK "got cursor" $
                         ioK (gestate,cursor')

    filePopupOutput ((filecmd,_),filename') =
        either menuCmd writeOtherFile filecmd
      where
        menuCmd filemenucmd =
	  case filemenucmd of 
	    Open   ->
		loadModuleK filename' alfstate' $
		either (showFileErr filename') $ \ (syntax',alfstate') ->
		gEditDo ([],startEditor alfstate' syntax') $
		updMenus alfstate' $
		editfile filename'
	      where
		alfstate' = setCurrentFile filename' (resetState alfstate)
	    Import ->
	      importModuleK filename filename' (snd gestate) alfstate $
	      either (showFileErr filename') $ \ (syntax',alfstate') ->
	      gEditDo ([],startEditor alfstate' syntax') $
	      updMenus alfstate' $
	      same
              {-	
	      hIOerr (ReadFile filename) showIOErr $
		      \(Str s)->gEditDo ([],importFile filename s) same
              -}
	    Browse -> browseFile filename filename'
	    SaveAs -> saveFileAs filename'
	    PrintToFile -> printToFile filename'
	    -- _ -> same

        writeOtherFile contents =
	    hIOerr (WriteFile filename' contents) showIOErr $
		 \ _ -> let msg = "Wrote to "++filename'
	                in --echoK msg $
			   gEditDo ([],editShowMessage (Right msg)) $
                           same


    fileCmd cmd = case cmd of
		    New    -> new
		    Open   -> ask
		    Import -> ask
		    Browse -> ask
		    Save   -> saveFileAs filename
		    SaveAs -> suggest filename
		    PrintToFile -> suggest (filename++".tex")
		    Quit   -> -- !! check for unsaved changes
			      hIOSucc (Exit 0) nullK
      where
        new = gEditDo ([],newFile unnamed) $
	      updMenus alfstate' $
	      ioKempty alfstate'
	  where alfstate' = resetState alfstate

	ask = ask' Nothing
	suggest = ask' . Just

        ask' optpath = putFilePopup (Left cmd,(show cmd,optpath)) same

    saveFileAs filename' =
	 hIOerr (WriteFile filename' (toFile contents)) showIOErr $
		 \ _ -> let msg = "Saved "++filename'
	                in --echoK msg $
			   gEditDo ([],changeState (setCurrentFile filename')) $
			   gEditDo ([],editShowMessage (Right msg)) $
                           same
			 
      where contents =  prSyntax file ++ "\n" ++
			prAlfState alfstate

    printToFile filename' =
	 hIOerr (WriteFile filename' ({-toFile-} contents)) showIOErr $
		 \ _ -> let msg = "Printed to "++filename'
	                in --echoK msg $
			   gEditDo ([],editShowMessage (Right msg)) $
                           same
      where
        contents =
	  "% by "++alfa_version++"\n\n"++
	  UPr.prTopSyntax (altdispfs alfstate) (drawOptions alfstate) file
	declDetail = V.get alfDeclDetail alfstate

    editCmd = {- either newDeclCmd -} otherEditCmd
      where
        --newDeclCmd names = gEditDo ([],newDecl names) $ same
	otherEditCmd cmd =
	    case cmd of
	      Undo -> putAlf GEditUndo same
	      Redo -> putAlf GEditRedo same
	      Copy' -> putSelection (Sel (stringRep,syntax)) $ same
		where
		  stringRep = prSyntax syntax
	      Paste' -> edit pasteSomething
	      AppendDecl -> gEditDo ([],appendDecl) same
	      Delete -> edit deleteSomething
	      Give -> edit giveSomething
	      Refine -> edit (refineSomething dOpts)
	      _ -> same -- other commands not implemented yet
	   where
	     edit e = gEditDo (path,e syntax) same

    viewCmd cmd =
      case cmd of
        Hide on           -> change' alfHiding      on
	Compact on        -> change' alfCompact     on
	UnfoldGoals on    -> change' alfUnfoldGoals on
	LayoutStyle d     -> change' alfLayoutStyle d
	DeclDetail d      -> change' alfDeclDetail  d
	PStyle pst        -> change' alfProofStyle  pst
	HideTrivialLet on -> change' alfHideTrivialLet on
	FontSize n        -> change' alfFontSize    n
	Redraw            -> gEditChangeState id $ same
	ZoomIn            -> gEditZoomIn $ same
	ZoomOut           -> gEditZoomOut $ same

    change' var value =
      if V.get var alfstate/=value
      then gEditChangeState (V.set var value) $ same
      else --echoK "no change" $
           same

    optionCmd cmd =
      case cmd of
        InputMode m -> putAlf (GEditInputMode m) $ same
        MenuMode m  -> putAlf (GEditMenuMode m)  $ same
	MenuLang m -> change' menuLangVar (snd m)
	AutoAbstract on -> change' alfAutoAbstract on
	OnlyRefine on -> change' alfOnlyRefine on
	AutoSolve on -> change' alfAutoSolve on
        SimpleRefine on -> change' alfSimpleRefine on
	AutoScroll on -> putAlf (GEditAutoScroll on) $ same
	AutoNextGoal on -> putAlf (GEditAutoNextGoal on) $ same

    utilCmd cmd =
      case cmd of
        EvalWindow on -> putEvalWindow on same
	MenuWindow on -> gEditMenuWindow on $ same


    giveInput msg =
      echoK ("Unexpected message from Input Popup: "{-++show msg-}) $
      same

    layoutInput l = gEditDo ([],changeLayout l) same

    evalPopup on = putMenu (Utils (EvalWindow on)) same -- on==False always...
    gfxWindow _ = same

    selectionInput selmsg =
      echoK "Unexpected message from AlfSelectionF" $ same

    alfOutput geoutput =
      case geoutput of
        GEditMessage msg -> echoK msg $ same
        GEditChange gestate' -> editchange gestate'
	GEditSelect cursor' -> editselect cursor'
	GEditMenuWindowOff -> putMenu (Utils (MenuWindow False)) same
	GEditEdit _ (_,EditLayout l) -> putEditLayout l $ same

	GEditEdit _ (path,Paste pasteOp) ->
	    putSelection PasteSel $
	    waitForK (getSelection fromSel) $ \ something ->
	    gEditDo (path,pasteOp something) $
	    same
	  where
	    fromSel (SelNotify x) = Just x
	    fromSel _ = Nothing

	GEditEdit _ (path,RefineWithText multiline msg promptparse compl) ->
	    getGivenInput multiline msg promptparse compl $ \ edit ->
	    gEditDo (path,edit) same
	GEditEdit _ (_,BrowseFile filename') ->
	  browseFile filename filename'
	GEditEdit _ (_,SaveFileAs optpath contents) ->
          putFilePopup (Right contents,("Save",Just (fromMaybe "" optpath))) same
	GEditEdit _ (_,DisplayGfx gfx) ->
          putGfxWindow gfx same
	_ -> same

    browseFile filename filename' =
	browseModuleK filename filename' (resetState alfstate) $
	either (showFileErr filename) $ \ fsa@((absfilename,_),_) ->
	putBrowser fsa $
	putBrowserTitle absfilename $
	same

    fromBrowser (filename,cmd) =
      case cmd of
	Br.Browse -> putFilePopup (Left Browse,("Browse",Just filename)) same
	Br.Open -> filePopupOutput ((Left Open,()),filename)
	Br.Pick s ->
          case s of
	    ImportS (U.Import filename') -> browseFile filename filename'
	    _ -> gEditDo' (path,flip (clickRefineByCopying dOpts) s) same
        -- (Fixed) optannot=Nothing! refine in ND style won't work! 

    gEditDo (path,editfunc) = gEditDo' (path,const editfunc)
    gEditDo' msg = putAlf (GEditDo msg)
    gEditChangeState upd = putAlf (GEditChangeGlobalState upd)
    gEditZoomIn = putAlf GEditZoomIn
    gEditZoomOut = putAlf GEditZoomOut
    gEditMenuWindow on = putAlf (GEditMenuWindow on)
    --browserDo msg = putBrowser (GEditDo msg)
    --showErr err = appendChanK stderr (show err) same
    showErr err = gEditDo ([],editError err) same
    --showFileErr filename err = showErr (filename++": "++err)
    showFileErr filename = showErr -- file name is added elsewhere now
    showIOErr = showErr.show

    getGivenInput multiline prompt def compl cont =
        putGiveInput (to (Just prompt,def,compl)) $
        waitForK (getGiveInput . from $ yes) $ \ opt_s ->
	case opt_s of
	  Just s -> cont s
	  _ -> same
      where (to,from) = if multiline then (Left,left) else (Right,right)

    updMenus state = updViewMenu state . updOptionsMenu state

    updViewMenu state =
        upd (Hide . V.get alfHiding) .
	upd (Compact . V.get alfCompact) .
	upd (LayoutStyle . V.get alfLayoutStyle) .
	upd (DeclDetail . V.get alfDeclDetail) .
	upd (PStyle . V.get alfProofStyle) .
	upd (HideTrivialLet . V.get alfHideTrivialLet) .
	upd (UnfoldGoals . V.get alfUnfoldGoals) .
	upd (FontSize . V.get alfFontSize)
      where 
        upd vopt = putMenu (View (vopt state))

    updOptionsMenu state  =
        upd (AutoSolve . V.get alfAutoSolve) .
	upd (AutoAbstract . V.get alfAutoAbstract) .
	upd (OnlyRefine . V.get alfOnlyRefine) .
	upd (SimpleRefine . V.get alfSimpleRefine) .
	upd (MenuLang . menulangs)
      where
	upd oopt = putMenu (Options (oopt state))
	menulangs state = (langs,lang')
	  where
	    langs = BuiltinLang:map (PluginLang . fst) (altdispfs state)
	    lang = V.get menuLangVar state
	    lang' = if lang `elem` langs then lang else BuiltinLang

putTitle = putK. High. Right
filePopupTag = High . Left . Right . Left . Right . Left

{-
tstF = (stdoutF>=^<show>+<alfEditF) >==<
       (snd>^=<readFileF) >==<
       putsF args (inputDoneSP >^^=< shellF "Input file" filePickF)

showitF = stdoutF >=^<  (sh.snd)
  where sh (DeclsS decls) = show_decls decls
        sh s = typeof s
-}

no _ = Nothing
yes (_,s) = Just s
left f = either f no
right = either no
high = message no

theory _ = ()


alfa_version = "Alfa/"++agda++" version "++compileDate++" ("++compiler++")"
  where compiler =
#ifdef __HBC__
           "HBC"
#elif defined(__GLASGOW_HASKELL__)
           ("GHC "++show __GLASGOW_HASKELL__)
#else
           "unknown compiler"
#endif
        agda =
#ifdef IAGDA
                "IAgda"
#else
                "Agda"
#endif
