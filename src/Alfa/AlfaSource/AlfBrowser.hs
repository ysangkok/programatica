module AlfBrowser(alfBrowserF,BrowserOutput(..)) where
import Fudgets
import Fud
import ContribFudgets(wmShellF',menuBarF,menu,cmdItem,radioGroupItem,item,key,idT,Transl(..))
import Gedit(build,stdSelect)
import AlfEditorOps(drawAlfTopSyntax)
import EditAnnot
import AlfState(changeOptions,alfDeclDetail,DeclDetail(..))
import AlfSyntax(Syntax)
import Variables

-- If Haskell only had first class constructors...
#define CON(c) (Transl (c) (\x->case x of c y->Just y;_->Nothing))

data BrowserOutput
  = Open        -- open the current file in the editor
  | Browse      -- ask user for another file to browse
  | Pick Syntax -- user clicked on something, refine or browse
  --deriving (Eq)

instance Eq BrowserOutput where
  Open==Open = True
  Browse==Browse = True
  Pick _==Pick _ = True
  _ == _ = False

data BrowserState dr cur
  = St { filename::FilePath,
         drawing::dr,
	 optcursor::Maybe cur
       }

state0 = St "" (blankD 10) Nothing
details0 = NamesAndTypes

alfBrowserF = delayF $ auxShellF "Alfa Browser" browserF
  where
    browserF =
      revVBoxF $
      loopThroughRightF (mapstateF ctrl state0) dispF
      >==< collectF details0 menubarF

    dispF = scrollF gfxDispF
      where
	gfxDispF = graphicsF' custom
	custom = setGfxEventMask [GfxButtonMask,GfxMotionMask] .
		 setAdjustSize False
		 --setSizing Dynamic .
		 --setInitDisp (blankD 10)
    menubarF = spacer (menuBarF menuBar)
      where
        menuBar = [item fileMenu "File",item viewMenu "View"]
	fileMenu = menu CON(Left)
	    [cmdItem Browse "Browse..." `key` "b",
	     cmdItem Open   "Open this file in the editor" `key` "o"]
	viewMenu = menu CON(Right)
	    [radioGroupItem idT viewOptions details0 "Declaration Details"]
	viewOptions =
    	    [item JustNames "Just Names",
	     item NamesAndTypes "Type signatures",
	     item CompleteDecls "Complete definitions"]
	spacer = spacer1F (hAlignS aLeft `compS` noStretchS True True)

    ctrl st =
        either gfxevent (either fromFileMenu newgfx)
      where
        gfxevent e =
	  case e of
	    GfxButtonEvent {gfxType=t@Pressed,gfxPaths=((path,_):_)} ->
	        (st,[Right (filename st,Pick syntax)])
	      where syntax = build (drawingPart (drawing st)
				    (stdSelect (drawing st) path))
	    GfxMotionEvent {gfxPaths=((path,_):_)} ->
	        (st{optcursor=optcursor'},curcmds)
	      where
	        optcursor' = Just cursor'
	        cursor' = stdSelect (drawing st) path
		curcmds =
		  if optcursor' == optcursor st
		  then []
		  else rmOldCursor++addNewCursor
		addNewCursor = [cursorAt cursor' True]
		rmOldCursor =
		  case optcursor st of
		    Just cursor  -> [cursorAt cursor False]
		    _ -> []
		cursorAt path on = Left (ChangeGfx [(path,(on,Nothing))])
	    _ -> (st,[])

	fromFileMenu cmd = (st,[Right (filename st,cmd)])

	newgfx (((filename',syntax),alfstate),d) =
	    (st{filename=filename',
	        drawing=drawing',
		optcursor=Nothing},
	     [Left (replaceAllGfx drawing')])
	  where
	    drawing' = drawAlfTopSyntax alfstate' (syntax ::Syntax)
	    alfstate' = set alfDeclDetail d alfstate

{-
pairWithF :: F a b -> F a (a,b)
pairWithF f = postSP0 >^^=< throughF f
  where
    postSP0 = getSP $ either (const postSP0) postSP
    postSP i = getSP $ either outSP postSP
      where outSP o = putSP (i,o) $ postSP i
-}

collectF s fud = idLeftSP (collectSP s) >^^=< distr >^=< mergeF fud

distr = either (either Left (Right . Left)) (Right . Right)

mergeF fud = idRightF fud >=^< Right

collectSP = postSP0
  where
    postSP0 s = getSP $ either postSP0 (outSP1 s)
    outSP1 s i = outSP i s
    outSP i s = putSP (i,s) $ postSP i s
    postSP i s = getSP $ either (outSP i) (outSP1 s)


auxShellF title fud =
    filterRightSP >^^=< wmShellF' pm title fud >=^^< concatMapSP pre
  where
    pm = setDeleteWindowAction (Just DeleteUnmap)
    pre (Right x) = [Right x,Left (Right True)]
    pre (Left title) = [Left (Left title)]

