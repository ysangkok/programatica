module Main where
import Fudgets
import Fud
import Graphic
import Drawing
import FixedDrawing
import FlexibleDrawing(lpar,rpar,FlexibleDrawing)
import Bitmaps
--import DrawingOps
import MeasuredGraphics(GCtx)
import MeasuredGraphics(MeasuredGraphics)
import Gedit
import Gutils
import GCAttrs
import Annot
import Trace(trace)

main = fudlogue (shellF "Tst2" tstF)

tstF = gEditF (error "a") stdSelect decsHoleD []

numExpMenu =
   [(const $ litExpD $ show i,"Refine with "++show i) | i<-[0..9]]

idMenu d clearD = idRefineMenu ""
  where
    idAnnot s = annot' (d,(const clearD, "Delete"):idRefineMenu s)
    idRefineMenu s =
       let cs = ['a'..'z']++['A'..'Z']++(if null s then "" else ['0'..'9'])
       in [let s' = s++[i]
           in (const $ idAnnot s' $ var s',"Refine id with "++[i]) | i<-cs]
    var = vCenter . v

appD = binopD " " -- inefficient !!
plusD = binopD "+"
multD = binopD "*"
ltD = binopD "<"
eqD = binopD "="
gtD = binopD ">"

expAnnot = annot' ("Exp",clearExpMenu)
  where clearExpMenu = [(const expHoleD,"Delete")]

litExpD  = expAnnot . vCenter . l
leafExpD = expAnnot . vCenter . v 
expHoleD = annot' ("Place for an Exp",refineExpMenu) holeD
  where
    refineExpMenu =
      [(const appD,"Refine with Application"),
       (const plusD,"Refine with +"),
       (const multD,"Refine with *"),
       (const ltD,"Refine with <"),
       (const eqD,"Refine with ="),
       (const gtD,"Refine with >"),
       (const ifD,"Refine with if"),
       (const letD,"Refine with let")]
      ++idMenu "Exp" expHoleD
      ++numExpMenu

{-
ifD = expAnnot $
      (vbox''.map hbox') [[k "(if", expHoleD],
                       [k "then",expHoleD],
		       [k "else",expHoleD,k ")"]]
-}

ifD = expAnnot $
      hbox [g lpar,table 2 [k "if",expHoleD,
                            k "then",expHoleD,
			    k "else",expHoleD], g rpar]

letD = expAnnot $
       hbox [g lpar,table 2 [upk "let",decsHoleD,k "in",expHoleD],g rpar]

binopD s = expAnnot $
           west $
           hbox [g lpar,expHoleD,k s,expHoleD,g rpar]

decsHoleD = annot' ("Decs",[(newDec,"New Declaration After Last")]) $
            vbox'' [decHoleD]
  where
    newDec (AnnotD a (PlacedD placer (ComposedD decs))) =
      AnnotD a (PlacedD placer (ComposedD (decs++[decHoleD])))
    newDec d = d

decHoleD = annotp ("Dec",[(newDec,"New Declaration Below"),
                          (delDec,"Delete")]) $
           west $ hbox [lhsHoleD,k "=",expHoleD]
  where
    --newDec decD = {- annot' ("Decs",[]) $ -} vbox'' [decD,decHoleD]
    newDec [p] (AnnotD a (PlacedD placer (ComposedD decs))) =
      let (decs1,decs2) = splitAt p decs
      in AnnotD a (PlacedD placer (ComposedD (decs1++decHoleD:decs2)))
    newDec ps d = tr ps $ d -- !!! something went wrong...

    delDec [p] d@(AnnotD a (PlacedD placer (ComposedD decs))) =
      case splitAt (p-1) decs of
        (decs1,d:decs2) -> AnnotD a (PlacedD placer (ComposedD (decs1++decs2)))
	_ -> tr p $ d
    delDec ps d = tr ps $ d -- !!! something went wrong...

    tr x = trace (show x)

leafLhsD = annot' ("Lhs",clearLhsMenu) . vCenter . v
  where clearLhsMenu = [(const lhsHoleD,"Delete")]
lhsHoleD = annot' ("Place for Lhs",idMenu "Lhs" lhsHoleD) $ vCenter $ holeD

annot' (x,y) = annot (x,error "annot'",map (asnd (pairwith G)) y)

annotp (x,y) = annot2 (x,error "annotp",[(e f,(d,G d)) | (f,d)<-y])
  where e = EditParent . (Left.)

k = vCenter. upk
upk = fontnG keyWordFont.g
v = fontnG varFont.g
l = fontnG litFont.g

varFont = argKey "varfont" "-*-new century schoolbook-medium-i-*-*-13-*-*-*-*-*-iso8859-1"
keyWordFont = argKey "keywordfont" "-*-new century schoolbook-bold-r-*-*-13-*-*-*-*-*-iso8859-1"
litFont = argKey "keywordfont" "-*-new century schoolbook-medium-r-*-*-13-*-*-*-*-*-iso8859-1"

vbox'' = vbox.map (spacedD $ hAlignS aLeft)
vCenter = spacedD $ vAlignS aCenter
