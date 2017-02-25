module HtmlFormF(htmlFormF,FormReq(..),FormResp(..)) where
import AllFudgets hiding (untaggedListF{-,loopThroughRightF-})
import UserLayoutF
import Html(Html(..),HtmlItem)
import HtmlFormSubmit
import HtmlForms
import SelectF(selectF)
import Utils2(space,strToLower)
import ListUtil(assoc)
import HO(apSnd)
import DialogueIO
import ContinuationIO(stdout)
import Prelude hiding (IOError)

data FormReq = NewForms Html
             | FormPlaces [Rect]
	     deriving (Eq,Show)

data FormResp = SubmittedForm (InputMsg (String,HttpMethod))
              | FormSizes [Size]
	     deriving (Eq,Show)

data FormMsg = Reset
             | Submit
	     | RadioChange (String,String) -- (name,value)
	     | Output [(String,String)]
             deriving (Eq,Show)

dummyOutput = Output [] -- output from buttons and unimplemented things

htmlFormF :: F FormReq FormResp
htmlFormF =
    --echoIdF >==<
    (post >^=< dynF nullF >=^< pre)
  where
    pre (NewForms html) = Left. formsF.extractForms $ html
    pre (FormPlaces ps) = Right (Left ps)
    post (Left sizes) = FormSizes sizes
    post (Right formdata) =
      (SubmittedForm. inputMsg.submitForm.apSnd concat) formdata

--echoF = stdoutF>=^<((++"\n"). show)
--echoIdF = bypassF echoF

formsF forms = collectLayoutF n (untaggedListF (map formF forms))
  where
    n = sum . map (length.snd) $ forms

formF (attrs,[FormIsIndex]) =
    (pair attrs.(:[]).(:[]).pair "") `postMapSP` inputDoneSP >^^=< strF
  where
    strF = {-autoLayoutF $-} startupF [""] $ oldStringF (space 30) 
formF (attrs,form) =
    pair attrs `postMapSP`
    collectSP n >^^=<
    loopLeftF (post>^=<untaggedListF (map formElemF form)>=^^<concmapSP pre)
  where
    post (Output x) = Right x
    post formmsg    = Left formmsg
    pre  (Left formmsg) = [(i,formmsg)|i<-ns]
    pre  _ = []
    n = length form
    ns = [1..n]

untaggedListF fs = snd>^=<listF (number 1 fs)

formElemF :: HtmlInput -> F FormMsg FormMsg
formElemF inp =
    --showCommandF ("2"++iname) $
    autoLayoutF $
    sepF 3 $
    (if argFlag "bufbug" False
    then hIOSuccF (AppendChan stdout (iname++"\n"))
    else id) $
    --showCommandF ("1"++iname) $ 
    f
  where
    iname = name (htmlInputAttrs inp)
    f = case inp of
          FormInput attrs -> formInputF inp attrs
	  FormSelect attrs options ->
	      formSelectF (name attrs) multiple size options
	    where multiple = assoc id "no" attrs "MULTIPLE"/="no"
		  size = assoc read 1 attrs "SIZE"
	  FormTextArea attrs s ->
	      editWrapF (name attrs) s $ oldScrollF False (size,size) editorF
	    where size = Point (6*w) (13*h)
	          w = val 60 "COLS"
		  h = val 5 "ROWS"
		  val d a = assoc read d attrs a

formSelectF name multiple size options =
  case (multiple,size) of
    (False,1) ->
        wrapF name def $
	  loopLeftF $
	    toBothF >==<
	    simpleMenuF menuFont longest optstrings id >=^<
	    stripEither
      where
        def = assoc id (head optstrings) options True
	longest = (snd . maximum . map (swap.pairwith length)) optstrings
    _ -> listWrapF name def (map snd>^=<selectF multiple id>=^<upd)
      where
        def = [opt | (True,opt) <- options]
	upd sel = map (swap.pairwith (`elem` sel)) optstrings
	  -- since "sel == def" will hold, so will "upd sel == options"
  where
    optstrings = map snd options
		  
formInputF inp attrs =
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
      wrapF name (value "") $
        stripInputMsg>^=<
	f (space (val 20 "SIZE")) -- size could be cols,rows !!

    formCheckboxF = checkboxWrapF name (value "on") checked $ toggleF
    formRadioF = radioWrapF name (value "on") checked $ toggleF

    toggleF = toggleButtonF ""

    formSubmitF  = formActionF Submit "Submit" [([],"Return"),([],"Enter")]
    formResetF   = formActionF Reset  "Reset"  [([],"Escape")]

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

    dispF = labelF (show inp)


submitWrapF output f =
    concatMapAccumlSP submit dummyOutput>^^=<throughF f
  where
    -- A submit buttons name & value should be output only if
    -- this particular submit button is used to submit the form.
    submit out (Right Submit) = (dummyOutput,[out]) -- form being submitted
    submit _ (Left Submit) = (output,[Submit]) -- this submit button was pressed
    submit _ (Left Reset) = (dummyOutput,[Reset]) -- this is a reset button...
    submit out _ = (out,[])

wrapF = gWrapF (\n v -> [(n,v)])
listWrapF = gWrapF (map.pair)

gWrapF g name def f = loopThroughRightF (absF resetSP) f
  where
    resetSP = putSP (Left def) $ wrapSP def

    wrapSP v =
      getSP $ \ msg ->
      case msg of
        Left v' -> wrapSP v'
	Right Reset -> resetSP
	Right Submit -> putSP (Right (Output (g name v))) $ wrapSP v
	_ -> wrapSP v

checkboxWrapF name value def f = loopThroughRightF (absF resetSP) f
  where
    resetSP = putSP (Left def) $ wrapSP def

    wrapSP v =
      getSP $ \ msg ->
      case msg of
        Left v' -> wrapSP v'
	Right Reset -> resetSP
	Right Submit -> putSP (Right (Output (if v
	                                   then [(name,value)]
					   else []))) $ wrapSP v
	_ -> wrapSP v

radioWrapF name value def f = loopThroughRightF (absF resetSP) f
  where
    resetSP = set def $ wrapSP def
    set = putSP.Left

    wrapSP v =
      getSP $ \ msg ->
      case msg of
        Left v' | v'/=v -> putSP (Right (RadioChange (name,value))) $ wrapSP v'
	            -- If a radio button is turned off (v'==False),
		    -- this msg will turn it back on.
	Right Reset -> resetSP
	Right Submit -> putSP (Right (Output (if v
	                                   then [(name,value)]
					   else []))) $ wrapSP v
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
    resetSP = putsSP (load def) $ wrapSP
    load s = [Left c|c<-selectall++[EditReplace s]]

    wrapSP =
      getSP $ \ msg ->
      case msg of
        Right Reset -> resetSP
	Right Submit -> putSP (Left EditGetText) $ wrapSP
	Left (EditText s) -> putSP (Right (Output [(name,s)])) $ wrapSP
	_ -> wrapSP

name attrs = assoc id "noname" attrs "NAME"

collectSP 0 = nullSP -- hmm, happens if a form is empty
collectSP n = colSP n []
  where
    colSP 0 acc = putSP (reverse acc) $ collectSP n
    colSP n acc =
      getSP $ \ x ->
      colSP (n-1) (x:acc)

htmldebug = argFlag "htmldebug" False

--loopThroughRightF f1 f2 = loopCompThroughRightF (f1>+<f2)
