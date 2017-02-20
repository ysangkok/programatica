module HtmlFormF2(
	FormInput(..),FormOutput(..),FormMsg(..),
	formTextAreaF,formSelectF,formInputF
	) where
import AllFudgets hiding (untaggedListF{-,loopThroughRightF-})
import Html(Html(..),HtmlItem)
import HtmlFormSubmit
import SelectF(selectF)
import Utils2(space,strToLower)
import Data.ListUtil(assoc)
import HO(apSnd)
--import DialogueIO
--imaport ContinuationIO(stdout)
--import Prelude hiding (IOError)

type FormInput  = FormMsg
type FormOutput = FormMsg

data FormMsg
  = Reset
  | Submit
  | RadioChange (String,String) -- (name,value)
  | Output [(String,String)]
  deriving (Eq,Show)

dummyOutput = Output [] -- output from buttons and unimplemented things

formTextAreaF attrs s =
    editWrapF (name attrs) s $ oldScrollF False (size,size) editorF
  where size = Point (6*w) (13*h)
	w = val 60 "COLS"
	h = val 5 "ROWS"
	val d a = assoc read d attrs a

formSelectF attrs options0 =
  -- !!! Bug: should output the VALUE attribute of the selected option(s),
  -- if present, otherwise the option text.
  case (multiple,size) of
    (False,1) ->
        wrapF myname def $
	  loopF $ simpleMenuF menuFont longest optstrings id
      where
        def = assoc id (head optstrings) options True
	longest = (snd . maximum . map (swap.pairwith length)) optstrings
    _ -> listWrapF myname def (map snd>^=<selectF multiple id>=^<upd)
      where
        def = [opt | (True,opt) <- options]
	upd sel = map (swap.pairwith (`elem` sel)) optstrings
	  -- since "sel == def" will hold, so will "upd sel == options"
  where
    options = [("SELECTED" `elem` map fst attrs,opt) | (opt,attrs)<-options0]
    optstrings = map snd options
    multiple = assoc id "no" attrs "MULTIPLE"/="no"
    size = assoc read 1 attrs "SIZE" -- !!
    myname = name attrs

formInputF attrs =
    case typ of
      "text"     -> formTextF
      "password" -> formPasswordF
      "checkbox" -> formCheckboxF
      "radio"    -> formRadioF
      "submit"   -> formSubmitF
      "reset"    -> formResetF
      "hidden"   -> formHiddenF
      _          -> formUnimplF
  where
    typ = strToLower (attr "text" "TYPE")
    name = attr "noname" "NAME"
    value d = attr d "VALUE"
    checked = attr "no" "CHECKED"/="no"

    val d = assoc read d attrs
    attr d = assoc id d attrs

    formTextF = strF oldStringF
    formPasswordF = strF oldPasswdF
    
    strF f =
      iWrapF name (value "") $
	f (space (val 20 "SIZE")) -- size could be cols,rows !!

    formCheckboxF = checkboxWrapF name (value "on") checked toggleF'
    formRadioF    = radioWrapF name (value "on") checked toggleF'

    toggleF'      = fromLeft >^=< toggleF False [] nullF >=^< Left

    formSubmitF   = formActionF Submit "Submit" [([],"Return"),([],"Enter")]
    formResetF    = formActionF Reset  "Reset"  [([],"Escape")]

    formActionF msg deflbl hotkeys =
	submitWrapF output $ const msg>^=<buttonF lbl>=^^<nullSP
        --submitWrapF output $ const msg>^=<buttonF' (setKeys hotkeys) lbl>=^^<nullSP
	-- problems with hotkeys: interference with editorF, multiple forms...
      where
        lbl = value deflbl
	output = if name=="noname" then dummyOutput else Output [(name,lbl)]
	  -- hmm. Use of "noname"? Output from Reset button?
    formHiddenF = formUnimplF -- to uncover the secrets if htmldebug is on

    formUnimplF = 
      if htmldebug
      then wrapF name (value "") $ dispF
      else wrapF name (value "") $ nullLF

    dispF = labelF ("INPUT "++show attrs)


submitWrapF output f =
    putSP dummyOutput (concatMapAccumlSP submit dummyOutput)
     >^^=< throughF f
  where
    -- A submit buttons name & value should be output only if
    -- this particular submit button is used to submit the form.
    submit out (Right Submit) = (dummyOutput,[out]) -- form being submitted
    submit _ (Left Submit) = (output,[Submit]) -- this submit button was pressed
    submit _ (Left Reset) = (dummyOutput,[Reset]) -- this is a reset button...
    submit out _ = (out,[])


wrapF = gWrapF (\n v -> [(n,v)])
listWrapF = gWrapF (map.pair)
iWrapF = iWrapF' (\n v -> [(n,v)])

checkboxWrapF name value checked = gWrapF g name checked
  where g n v = if v then [(n,value)] else []

gWrapF g name def fud = iWrapF' g name def (inputChange>^=<fud)

iWrapF' g name def fud = loopThroughRightF (absF resetSP) fud
  where
    resetSP =
      putSP (Left def) $
      submitSP def  -- announces our presence

    submitSP v =
      putSP (Right (Output (g name v))) $
      wrapSP v

    wrapSP v =
      getSP $ \ msg ->
      case msg of
        Left imsg ->
	  case inputDone imsg of
	    Just _ -> putSP (Right Submit) $ wrapSP v'
	    _ -> wrapSP v'
	  where v' = stripInputMsg imsg
	Right Reset -> resetSP
	Right Submit -> submitSP v
	_ -> wrapSP v

radioWrapF name value def f = loopThroughRightF (absF resetSP) f
  where
    resetSP = set def $ submitSP def
    set = putSP.Left

    submitSP v =
      putSP (Right (Output (if v then [(name,value)] else []))) $
      wrapSP v

    wrapSP v =
      getSP $ \ msg ->
      case msg of
        Left v' | v'/=v -> putSP (Right (RadioChange (name,value))) $ wrapSP v'
	            -- If a radio button is turned off (v'==False),
		    -- this msg will turn it back on.
	Right Reset -> resetSP
	Right Submit -> submitSP v
        Right (RadioChange (name',value')) ->
	  if name'==name
	  then let v' = value'==value
	       in if v'/=v
	          then set v' $ wrapSP v'
		  else wrapSP v
	  else wrapSP v
	_ -> wrapSP v

editWrapF name def f = loopThroughRightF (absF resetSP) f
  where
    resetSP = putsSP (load def) $ submitSP def
    load s = [Left c|c<-selectall++[EditReplace s]]

    submitSP s =
      putSP (Right (Output [(name,s)])) $
      wrapSP

    wrapSP =
      getSP $ \ msg ->
      case msg of
        Right Reset -> resetSP
	Right Submit -> putSP (Left EditGetText) $ wrapSP
	Left (EditText s) -> submitSP s
	_ -> wrapSP

name attrs = assoc id "noname" attrs "NAME"

htmldebug = argFlag "htmldebug" False
