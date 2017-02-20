module ParsedStringPopupF where
import AllFudgets
import HO(apFst,apSnd)
import ContribFudgets(endButtonsF,smileyF',SmileyMode(..))
import ContribFudgets(stdcc,completionStringF'')

parsedStringPopupOptF t = parsedInputPopupOptF parsedCompletionStringF t
parsedTextPopupOptF t = parsedInputPopupOptF parsedTextF t

parsedStringPopupGroupOptF = parsedInputPopupGroupOptF parsedStringF

parsedInputPopupOptF parsedInputF title =
    --(apFst.apSnd) stripMaybe >^=<
    popupShellF' (setSizing Static) title Nothing (parsedInputF stdGuiF)

parsedInputPopupGroupOptF parsedInputF =
    popupGroupF (const 0,wattrs,nullK) (parsedInputF miniGuiF)
  where
    wattrs = [CWEventMask [ButtonReleaseMask]] -- to avoid propagating them

--parsedStringF :: Graphic gfx => ParsedInputF gfx a
parsedStringF guiF =
    parsedInputF (guiF (Right>^=<
			stringF'' standard >=^^<
			concatMapSP (either (either compl setpos) kludge)))
  where
    compl _ = []
    setpos (row,col) = [Left $ setCursorPos (col-1)]
    kludge s = [Right $ if '\n' `elem` s then unwords (words s) else s]

--parsedStringF :: Graphic gfx => ParsedInputF gfx a
parsedCompletionStringF guiF =
    parsedInputF (guiF (mapEither (map fst) id >^=<
			completionStringF'' stdcc standard >=^<
			either (either compl setpos) kludge))
  where
    compl = Left
    setpos (row,col) = Right $ Left $ setCursorPos (col-1)
    kludge s = Right $ Right $ if '\n' `elem` s then unwords (words s) else s

--parsedTextF :: Graphic gfx => ParsedInputF gfx a
parsedTextF guiF = parsedInputF (guiF editorF')
  where
    editorF' = mapFilterSP post >^^=< scrollF editorF >=^^< concatMapSP pre
    post (EditChange msg) = Just (Right (mapInp (++"\n") msg))
    post _ = Nothing
    pre (Right s) = loadEditor s
    pre (Left (Left compl)) = []
    pre (Left (Right pos)) = setEditorCursorPos pos
    --stretchF = noStretchF False False

type ParsedInputF g b = F (Maybe g, (Maybe String, String, String -> Either ((Int, Int), String) b, Bool)) (Maybe b)

stdGuiF inputF =
        spacerF (minSizeS (pP 500 110)) $
        vBoxF $ (promptF >+< inputF) >+< (feedbackF >+< endButtonsF)
      where
        feedbackF =
	    hBoxF (spacer1F vCenterS (smileyF' (setMargin 2)) >+< displayF' pm)
	  where pm = setSizing Dynamic
	promptF = displayF' pm
	  where pm = setBorderWidth 0.
	             setBgColor [bgColor, "white"].
		     setFont labelFont

miniGuiF inputF =
        --spacerF (minSizeS (pP 500 110)) $
        --vBoxF $
	(nullF >+< inputF) >+< (nullF >+< nullF)

parsedInputF guiF=
    loopThroughRightF (absF ctrlSP0) guiF
  where

    out = Right
    toLoop = Left
    toPromptF = toLoop . Left . Left
    toStringF = toLoop. Left . Right
    toCurPos = toStringF . Left . Right
    toStringCompl = toStringF . Left . Left
    toSmiley = toLoop.Right. Left . Left
    toDisplay = toLoop.Right. Left . Right

    ctrlSP0 = getSP $ either (const ctrlSP0) (pgmCtrl "")

    setup init@(optprompt,optdef,compl,msg) s cont =
      putSP (toStringCompl compl) $
      maybe id (putSP . toPromptF) optprompt $
      putSP (toDisplay msg) $
      flip (maybe (cont s)) optdef $ \ def ->
      putSP (toStringF (Right def)) $
      cont def

    pgmCtrl s (optprompt,(optdef,msg,parse,imm),compl) =
      setup (optprompt,optdef,compl,msg) s $ \ s' ->
      change s' parse imm

    change s parse False =
      putSP (toSmiley Indifferent) $
      ctrlSP s parse False
    change s parse imm =
      case parse s of
        Left _  -> putSP (toSmiley Indifferent) $
	           ctrlSP s parse imm
	Right x -> putSP (toSmiley Happy) $
	           --putSP (out (InputChange x)) $
		   ctrlSP s parse imm

    done s parse imm =
      case parse s of
        Left (pos,msg) ->
 	    putSP (toSmiley Sad) $
	    putSP (toDisplay msg) $
	    putSP (toCurPos (fixpos s pos)) $		  
	    ctrlSP s parse imm

	Right x -> putSP (toSmiley Happy) $
	           --putSP (out (inputMsg x)) $
		   putSP (out (Just x)) $
		   putSP (toDisplay "") $
		   ctrlSP s parse imm
		   
    ctrlSP s parse imm = idle
      where
        idle = getSP $ either fromLoop (pgmCtrl s)

        fromLoop =
 	  either (either fromPromptF
		         (either fromCompl
			         (userInput parse imm)))
		 (either fromFeedbackF fromEndButtonsF)
	fromPromptF _ = ignore
	fromFeedbackF _ = ignore
	ignore = idle

	fromEndButtonsF = either fromOK fromCancel
	fromOK _ = done s parse imm
	fromCancel _ = putSP (out Nothing) $ idle

        fromCompl ws = putSP (toDisplay (unwords ws)) idle

    userInput parse imm msg =
      case msg of
        InputDone "Escape" s -> putSP (out Nothing) $ ctrlSP s parse imm
	InputDone k s | k/=inputLeaveKey -> done s parse imm
	_ -> change (stripInputMsg msg) parse imm

---

-- Workaround for the combination of
--   parsers that return e.g., (maxInt,maxInt) for EOF, and
--   editorF, whose rudimentary cursor positioning commands
--   create lists of cursor motion command of length maxInt
--   to move the cursor there...
fixpos "" _        = (0,0)
fixpos s (row,col) = (row',col')
    where
      row' = min row rowcnt
      --row' = ctrace "row" row'' row''
      rowcnt = length ls
      ls = lines s
      col' = min col (1+length (ls !! (row'-1)))
      --col' = ctrace "col" col'' col''
