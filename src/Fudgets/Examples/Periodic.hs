import Fudgets

main = fudlogue $ cache periodicF

cache = id
--cache = cf "gccache" gCcacheF . cf "fstructcache" fstructcacheF .
--      cf "fontcache" fontcacheF . cf "colorcache" colorcacheF

cf s c = if argFlag s False then c else id

periodicF = 
   shellF "shellF" $ tableF 3 $ 
      ll "buttonF" (cF $ buttonF' (setKeys [([Mod1],"p")]) "Press me") >+<
      ll "toggleButtonF" (cF $ toggleButtonF "Run") >+< 
      ll "radioGroupF" (cF $ radioGroupF [(x,show x) | x <- [1..3]] 1) >+<
      ll "menuF"  (cF $ menuF "Menu" [(x,"Choice "++show x) | x <- [1..3]]) >+<
      ll "stringF" (vcF stringF) >+<
      ll "passwdF" (vcF passwdF) >+<
      ll "intF" (vcF intF) >+<
      ll "displayF" (startupF ["message"] $ vcF displayF) >+<
      ll "intDispF" (startupF [123] $ vcF intDispF) >+< 
      ll "labelF" (cF $ labelF "label") >+<
      ll "terminalF" terminal >+< 
      ll "editorF" (scrollF editorF) >+<
      ll "Popup" dialogMenuF >+<
      ll "quitButtonF" quitButtonF

ll s f = frameF False 10 $ hBoxF (blueLabelF s >+< frameF True 5 f)


terminal = vBoxF $ terminalF defaultFont 5 10 >==< 
	 inputDoneSP >^^=< labAboveF "Enter lines to terminalF" stringF

cF = spacerF centerS
vcF = spacerF vCenterS
blueLabelF = vcF . labelF' (setFgColor "blue3")
frameF down margin =  border3dF down 2 . spacerF (marginS margin)

--

dialogMenuF = hScrollF (displayF' (setInitDisp "?")) >=^<
              snd>==<
              popupsF>=^<
	      (`pair` ())>==<
	      menuF "Dialogs" menuItems
  where menuItems = map (pairwith show) popups

popups = [MessagePopup ..]

data Popups
  = MessagePopup
  | ConfirmPopup
  | StringPopup
  | PasswdPopup
  deriving (Eq,Ord,Show,Enum)

popupsF =
  listF [
    (MessagePopup,
     show>^=<messagePopupF>=^<const ["messagePopupF shows",
                                         "messages that can be",
					 "several lines long."]),
    (ConfirmPopup,
     show>^=<confirmPopupF>=^<
     const ["confirmPopupF shows a multi-line message that",
            "you should confirm or cancel."]),
    (StringPopup,
     show>^=<stringPopupF "default">=^<
     const (Just "stringPopupF asks you to enter a string",Nothing)),
    (PasswdPopup,
     show>^=<passwdPopupF "">=^<
     const (Just "passwdPopupF asks you to enter a password",Nothing))
  ]
