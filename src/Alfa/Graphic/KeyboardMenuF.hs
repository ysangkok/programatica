module KeyboardMenuF(keyboardMenuF,KMenu(..),KInput(..),KOutput(..)) where
import Fudgets
import ContribFudgets(delayedAuxShellF)
import Fud(westD)
import GPickListF
import Shortcuts
import DrawStatus
import Char
import ListUtil(mapSnd)

type KMenu a b = (KOptions,[(Either a b,(String,Gfx))])
type KOptions = (Bool,Bool) -- (enable keyboard shortcuts?,show status?)

type KInput a b i = Either (Either Bool (Either Char (KMenu a b))) i
type KOutput a b o = Either (Either Bool (Either a b)) o

keyboardMenuF :: KMenu a b -> F i o -> F (KInput a b i) (KOutput a b o)
keyboardMenuF m fud =
    createDrawStatus $ \ dr ->
    hBoxF $
    post>^=<
    (loopThroughBothF pickListMenuF (keyF dr) >+< popupMenuF (txtm m) fud)
    >=^^<concatMapSP pre
  where
    keyF dr = absF (keySP dr m `preMapSP` fromRight)
    txtm = mapSnd fst . snd
    --noPopupMenuF m f = nullF >+< f -- quick disabling of the popup menu
    post = either (either fromPickList fromKeys) (either fromPopup fromFud)
      where
        fromPickList = Left
	fromKeys = Left . Right
	fromPopup = Left . Right
	fromFud = Right
    pre = either (either popIn (either keyIn menuIn)) fudIn
      where
        popIn b = [toPickList (Left b)]
	keyIn key = [toKeys (Left key)]
	menuIn menu = [toKeys (Right menu),toPopup (txtm menu)]
	fudIn x = [toFud x]
    --pre (Left m@(Right menu))= [toPopup (txtm menu), toKeys m]
    --pre (Left k@(Left key)) = [toKeys k]
    --pre (Right x) = [toF x]
    toKeys=Left . Right
    toPickList=Left . Left
    toPopup = Right . Left
    toFud = Right . Right

    --pickListMenuF :: F [(a,(String,G))] a
{-
    pickListMenuF =
      (--shellNoQuitF "Menu" $
       --startupF [m] $
       (fst.snd)>^=<stripInputSP>^^=<
        noStretchF True False (pickListF snd)>=^<replaceAll)
--}
    pickListMenuF =
       --shellNoQuitF "Menu" $
       Right >^=<
	(delayedAuxShellF "Menu" $
	 snd>^=<noStretchF True False (gPickListF (tableP' 2 Horizontal 2) []))
	>=^< either Right id
--}

keySP dr ((sc,st),menu) = startSP
  where
    startSP = nextSP initmenu
    initmenu = shortcuts' sc menu
    nextSP menu = putSP (toPickList (disp st dr menu)) $ keySP' menu
    toPickList = Left
    out = Right
    keySP' menu = getSP $ either gotKey (keySP dr)
      where
	same = keySP' menu
	gotKey k =
	  case pick menu k of
	    [(_,(f,_))] -> putSP (out f) startSP
	    [] -> startSP -- !! error feedback to user?
	    menu' ->  nextSP menu'

--disp menu = [(f,"["++k++"] "++d) | (k,(f,(d,g)))<-menu]
disp st drawStatus menu =
    [(f,boxD [westD (addStatus f $ g (sctxt s)),disp' d]) | (s,(f,d))<-menu]
  where
    addStatus = if st then addStatus' else const id
    addStatus' f kD = hboxD' 2 [status f,kD]
    status = drawStatus . isRight
    disp' (s,d) = if textmenu then g s else atomicD d
    sctxt "" = " "
    sctxt s  = "["++s++"] "

pick menu c =
  case [(s,d) | (c':s,d)<-menu, c'==c] of
    [] -> [(s,d) | (c':s,d)<-menu, toLower c'==toLower c]
    m -> m

shortcuts' False = map ((,) "")
shortcuts' True = ordShortcuts str
  where str (_,(s,_)) = [c|c<-s,c/=' ']

--smallF = spacer1F (sizeS (pP 150 100))

--shellNoQuitF = shellF' (setDeleteQuit False . setInitPos Nothing)

textmenu = argFlag "textmenu" False
