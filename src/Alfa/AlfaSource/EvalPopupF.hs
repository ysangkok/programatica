module EvalPopupF(evalPopupF) where
import Maybe(listToMaybe)
import Fudgets
import ContribFudgets(delayedAuxShellF',vSplitF)
import Fud(noStretchS)
import qualified ProofEngine as PE
import EditMonad(runEd)
import AlfEditMonad(edPE)
import FileConv(expP)
import AlfSyntax
import InfoDispF
import UAbstract(Exp(..))
import UAnnots
import AbstractOps(splitAnnots)

import AlfOps(drawAlfSyntax,drawAlfTopSyntax,parseStrEd)
import AlfState(csps)
import Annot(ADrawing(..)) -- for type checking

-- Because of -fno-syn-expand:
import AlfState()

--expand = argFlag "expandevaltype" False

evalPopupF alfstate0 =
    fromLeft >^=<
    delayedAuxShellF' pm "Evaluate Expressions..." (promptEvalF >=^< pre)
  where
    pm = setSizing Static
    pre (alfstate,syntax) =
        case syntax of
          ExpS e -> case splitAnnots e of
		      (as,EMeta g) -> meta g
		      (as,_) -> maybe top meta (listToMaybe [g|Was (N g)<-as])
	  _ -> top
      where
	meta g = f ("?"++show g)   (Just g)
        top    = f "the top level" Nothing
        f ctx optMeta = ((prompt++ctx),(alfstate,optMeta))
	prompt = "...in the context of "

    promptEvalF = vBoxF (fromRight>^=<prodF promptF evalF)
    promptF =
      displayF' (setBorderWidth 0 . setBgColor bgColor . setFont labelFont)
    evalF = revVBoxF $
	    valDispF >=^^< mapstateSP ctrl (alfstate0,Nothing) >==<
            idRightF expInputF >=^< Right
      where
        expDispF =
    	   revHBoxF $
	   infoDispF (blankD (pP 120 30),(undefined,undefined)) >==< playBackF
        expInputF = "Expression" `labLeftOfF` (idF>==<stringInputF)
	valDispF = nullSP >^^=<
		   noStretchF False False (
		     vSplitF ("Type"  `labLeftOfF` expDispF)
		             ("Value" `labLeftOfF` expDispF)
		     >=^^< (splitSP `preMapSP` swap))

    ctrl state@(alfstate,optMeta) msg =
      case msg of
        Right newstate -> (newstate,[])
	Left s -> (state,[output])
	  where
	    output =
	      either err fst $ flip runEd alfstate $
	             do e <- parseStrEd expP s
		        t <- edPE $ PE.typeOf optMeta e
		        --e' <- edPE $ PE.compute optMeta e
			let ts = unf t
                            es = unf e
		        return (map prE es,map prE ts)
	    err s = ([prErr s],[prErr "Type error!"])
	    unf = cut . PE.unfolds (csps alfstate) 0 optMeta
	      where
		cut (x1:xs@(x2:_)) = if x1==x2 then [x1] else x1:cut xs
	        cut xs = xs

            prE e = (m $ drawAlfTopSyntax alfstate e,d)
	    prErr err = (m $ g err,d)
	    d =(drawAlfSyntax alfstate,drawAlfTopSyntax alfstate)
	    m = SpacedD (noStretchS False False `compS` marginHVAlignS 3 aCenter aCenter) . boxD . (:[])

playBackF =
    loopThroughRightF
      (mapstateF ctrl empty) 
      (placerF (spacerP vCenterS horizontalP) $
       listF [(s,buttonF s)|s<-["<<","<",">",">>"]])
  where
    empty = ([],[])
    ctrl tape = either (button . fst) input
      where
        input [] = (empty,[])
	input xs@(x:_) = (([],xs),[Right x])
	button "<<" =
	  case tape of (bs,fs) -> input (reverse bs++fs)
	button "<" =
	  case tape of
	    (x:bs,fs) -> ((bs,x:fs),[Right x])
	    _ -> (tape,[])
	button ">" =
	  case tape of
	    (bs,x:y:fs) -> ((x:bs,y:fs),[Right y])
	    _ -> (tape,[])
	button ">>" =
	  case tape of
	    (bs,fs@(_:_)) -> ((reverse (init fs)++bs,[x]),[Right x])
	      where x = last fs
	    _ -> (tape,[])
