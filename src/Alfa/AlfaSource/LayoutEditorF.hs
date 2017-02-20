module LayoutEditorF(layoutEditPopupF) where
import Fixity
import Fudgets
import DrawOptions(Name,ArgOptions(..),BitmapSource(..))
import Fonts(varFont,symbolFont,fontSizeS)
import Maybe(fromMaybe)

layoutEditPopupF =
    prepostMapHigh pre post (inputPopupF "Layout Editor" layoutEditorF Nothing)
  where
    pre (name,layout) = (Just (show (name::Name)),Just layout)
    post ((Just name,_),layout) = (read name::Name,layout)

layoutEditorF =
    inputMap post pre (inputPairLF Above hideNdispF fixityF)
  where
    hideNdispF = inputPairLF Above argHideF displayNfontF
    pre (ArgOptions hide fixity displayAs useSymbol) =
	 ((hide,(displayAs,useSymbol)),fixity)
    post ((hide,(displayAs,useSymbol)),fixity) =
	 ArgOptions hide fixity displayAs useSymbol

displayNfontF =
    inputPairSP >^^=< loopCompF partsF >=^^< splitSP
  where
    partsF = visualGroupF (displayAsF >+< fontF)
    displayAsF =
      prepostMapHigh (mapEither setFont' setStr) (Right . mapInp outStr)
	             ("Display As:" `labLeftOfF` stringF''')
    stringF''' = putF (inputChange "") $ stringF'' standard
    fontF = toBothSP >^^=< bitmapSourceF >=^< stripEither
    setStr = fromMaybe ""
    outStr "" = Nothing
    outStr s  = Just s
    setFont' bitmapSource =
      case stripInputMsg bitmapSource of
	UseSymbolFont -> setFont (symbolFont fontSizeS)
	UseNormalFont -> setFont (varFont fontSizeS)
	UseImageFile  -> setFont defaultFont
	

argHideF :: InF Int Int
argHideF = "Number of hidden arguments:" `labLeftOfF` int99F

--useSymbolF = inputChange >^=< putF False (toggleButtonF "Use Symbol Font")

bitmapSourceF =
    inputChange >^=< putF UseNormalFont (radioGroupF alts UseNormalFont)
  where
    alts = [(UseNormalFont,"Use Normal Font"),
	    (UseSymbolFont,"Use Symbol Font"),
	    (UseImageFile,"Use Image File")]

fixityF :: InF Fixity Fixity
fixityF =
    inputMap post pre (inputPairLF Above isInfixF assocPrecF)
  where
    pre (Infix assoc prec) = (dummyInfix,(assoc,prec))
    pre (Distfix3 assoc prec) = (dummyDistfix3,(assoc,prec))
    pre (Distfix3b assoc prec) = (dummyDistfix3b,(assoc,prec))
    pre (Distfix4 assoc prec) = (dummyDistfix4,(assoc,prec))
    pre (Distfix assoc prec) = (dummyDistfix,(assoc,prec))
    pre (Postfix prec) = (dummyPostfix,(NonAssoc,prec))
    pre fixity = (fixity,(NonAssoc,0))
    post (Infix _ _,(assoc,prec)) = Infix assoc prec
    post (Distfix3 _ _,(assoc,prec)) = Distfix3 assoc prec 
    post (Distfix3b _ _,(assoc,prec)) = Distfix3b assoc prec 
    post (Distfix4 _ _,(assoc,prec)) = Distfix4 assoc prec
    post (Distfix  _ _,(assoc,prec)) = Distfix  assoc prec
    post (Postfix _,(assoc,prec)) = Postfix prec
    post (fixity,_) = fixity
 
isInfixF :: InF Fixity Fixity
isInfixF = inputChange >^=< radioGroupF fixities Nonfix
  where
    fixities = [sh Nonfix,
	        sh Tuple,
	        sh Fraction,
	        (ProofGoal,"Proof style goal"),
		(Quantifier True,"Quantifier, show domain"),
		(Quantifier False,"Quantifier, hide domain"),
		(Big,"Big Operator"),
--		(BigQuantifier,"Big quantifier"),
		(dummyPostfix,"Postfix, unary operator"),
		(dummyDistfix3,"Infix ternary, operands above, left of and right of operator"),
		(dummyDistfix3b,"Infix ternary, operands below, left of and right of operator"),
		(dummyDistfix4,"Infix 4ary, operands above, below, left of and right of operator"),
		(dummyDistfix,"Mixfix operator, use _ in name"),
		(dummyInfix,"Infix binary operator")]

    sh x = (x,show x)

dummyInfix = Infix NonAssoc 0
dummyDistfix3 = Distfix3 NonAssoc 0
dummyDistfix3b = Distfix3b NonAssoc 0
dummyDistfix4 = Distfix4 NonAssoc 0
dummyDistfix = Distfix NonAssoc 0
dummyPostfix = Postfix 0

assocPrecF :: InF (Assoc,Precedence) (Assoc,Precedence)
assocPrecF =
  visualGroupF $
    "For infix operators only:" `labAboveF`
    inputPairLF Above assocF precedenceF

assocF :: InF Assoc Assoc
assocF = inputChange >^=< radioGroupF assocs NonAssoc
  where
    assocs = [(Assoc,"Associative"),
	      (LeftAssoc,"Left Associative"),
	      (RightAssoc,"Right Associative"),
	      (NonAssoc,"Non-associative")]

precedenceF :: InF Precedence Precedence
precedenceF = "Precedence (0-9):" `labLeftOfF` int99F

inputMap :: (o1->o) -> (i->i1) -> InF i1 o1 -> InF i o
inputMap post pre = prepostMapHigh pre (mapInp post)

int99F = spacer1F leftS intF --' (setInitSize "99")

visualGroupF f = border3dF True 1 (marginF defaultSep f) >=^< Right
