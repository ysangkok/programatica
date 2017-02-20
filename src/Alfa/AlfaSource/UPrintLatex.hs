{-# COMPILERFLAGS -fno-overload-restr #-}
module UPrintLatex(prTopSyntax) where
import UAbstract
import qualified UAbstract as U -- because of GHC 4.06 bug
import UAnnots
import AlfAbstract
import AlfaText
import AbstractOps
import AlfSyntax
import AlfModules(stripcomment)
--import NewPP
--import Fudgets(argReadKey)
import qualified DrawOptions as DO
import DrawOptions
import Fixity
import Mixfix
import Patterns
import Collect(collectBySnd)
import Utils2(apBoth)
import ListUtil(assoc,chopList)
import List(intersperse,groupBy)
import Maybe(fromMaybe)
import Char(isSpace)
import HO(apFst)
import PPrintTH
import Fud(argFlag)

-- debugging:
import Debug2(trace)

#ifndef __HASKELL98__
#define fmap map
#endif

quantifierDots = argFlag "quantifierdots" False
hidingInSignatures = argFlag "hiding_in_signatures" True

type IA = InheritedAttribute
data InheritedAttribute
  = IA { syn_ctx :: SyntacticContext (),
	 math_mode :: Bool,
	 --checked :: Bool,
	 --isPattern :: Bool,
	 --isCon :: Bool,
	 --font_size :: Int,
	 data_lhs :: Maybe Exp
	 --layoutdir :: Maybe LayoutDir
	}
  deriving (Show)

iaTop drawOpts = IA TopLevel False Nothing
-- {-True False False-} (fontSize drawOpts) --False True

prTopSyntax altdispfs options = prSyntax' altdispfs options (iaTop options)
--prSyntax altdispfs options a s = draw options (inherited a) altdispfs s
prSyntax' altdispfs options ia =
  pprint . cleanpunct . draw options ia altdispfs . syn

--wide options = options { maxWidth=16000 }

draw drawOpts@(DrawOptions
	         {compactOn=compactOn,
		  declDetail=declDetail,
		  layoutStyle=layoutStyle,
		  argOpts=GlobalArgOptions {hidingOn=hidingOn}})
     ia altdispfs s =
       --(drawSyntax,(((drawGoal,drawSolution),drawConstraint),drawAssumption))
    --drawSyntax s =
      case s of
	ExpS e -> drawExp' ia e
	--ExpsS es -> drawAExps ia es
	--CDeclS d -> drawCDecl d
	--CDeclsS ds -> drawCDecls ds
	DeclS d -> drawTopDecl ia d
	DeclsS ds -> drawTopDecls ia ds
	ImportS i -> drawImport ia i
	--ImportsS is -> drawImports is
	ModuleS m -> drawModule ia m
	--BranchS b -> draw
	BranchesS bs -> drawBranches ia bs
	--BranchS b -> drawBranch ia b
	ContextS ctx@(Ctx optlayout _) -> drawAlfCtxOpt optlayout ia ctx
	--ComS com -> drawCom ia com
	VarS var -> drawVar ia var
	ConS con -> drawCon ia con
	LabelS label -> drawLabel ia label
	SortS sort -> drawSort ia sort
	--HoleS -> decHole
	TypingS t ->  drawTyping' ia t
	--GoalS g -> drawGoal g
	--SolutionS s -> drawSolution s
	--ConstraintS c -> drawConstraint c
	--AssumptionS a -> drawAssumption a
	_ -> -- Prevent prog from crashing, just in case...
	     trace ("drawSyntax: "++typeof s) $
	     pr "\\ldots"
  where
    drawModule ia (Module decls) =
       "\\providecommand{\\alfaimport}[1]{{\\bf import } {\\it #1}}" !/
       "\\providecommand{\\alfadeclcomment}[1]{#1}" !/
       "\\providecommand{\\alfadeclannot}[1]{#1}" !/
       "\\providecommand{\\alfaexpcomment}[1]{#1}" !/
       drawTopDecls ia decls

    drawImport ia i@(Import filename) =
      -- k' ia "import " & v' ia filename
      "\\alfaimport{"!filename!"}"

    drawComment' ia comment =
      case stripcomment comment of
        '#':_ -> "\\alfadeclannot{"!cmnt comment!"}"
	s -> "\\alfadeclcomment{"!(mparbox ia $ pr s)!"}"
       -- comment is assumed to contain latex code

    drawTopDecls ia = prsep (nl!nl) . map (drawTopDecl ia)

    drawLetDecls ia = indented . drawDecls ia
      where drawDecls ia = mvboxD ia . map (drawDecl ia)

    drawTopDecl ia d = drawDecl ia d

    drawDecl :: IA -> Decl -> Document
    drawDecl ia cd@(Comment s) = drawComment' ia s
    drawDecl ia cd@(ImportDecl imp) = drawImport ia imp
    drawDecl ia cd@(Decl checked' defs) =
       "%%%" & wmap DO.nameStr (namesDecl cd) !/
       if math_mode ia 
       then errBar & "\\left [" !/ vboxlD (map (drawDef ia) defs) !/
	    "\\right."
       else vmap ((nl!/).drawDef ia) defs
      where
	errBar =
	  if checked' then nil
	  else nil --[fgD ["Red","Black"] $ g $ vFiller 5] -- !!

    --drawDef :: (Name,(Context,Exp,Exp))-> Drawing (Annot x y) G
    drawDef ia (DefA ps@(_,props,optDetails) def) =
        if null props
        then drawDefB ia optDetails def
        else drawProps props & "\\\\"!nl & drawDefB ia optDetails def

    drawProps = mmbox ia . small . wmap drawProp
      where drawProp = k' ia . show

    drawDefB ia optDetails def =
	case details of
	  DeclAsTextBy fn ->
	    case lookup fn altdispfs of
	      Just dr -> drawText ia (dr (syn def))
	      _ -> std
	  DeclAltViewsBy fns ->
	      mvboxD ia (std:
		      [drawText ia (dr (syn def))
		       | Just dr<-map (flip lookup altdispfs) fns])
	  _ -> std
      where
	std = mdrawDefBstd ia details def
        details = fromMaybe declDetail optDetails

    mdrawDefBstd :: IA -> DeclDetail -> DefB -> Document
    mdrawDefBstd ia details def =
      case def of
        CommentDef s -> drawComment' ia s
	_ -> {-drmath-} drawDefBstd ia (details, def)

    drawDefBstd :: IA -> (DeclDetail,DefB) -> Document
    drawDefBstd ia (details, def) =
      --trace ("drawDefBstd "++show ia) $
      case def of
        CommentDef s -> drawComment' ia s
        Package (n,(ctx,b)) ->
	  vboxlD'' [
	     k' ia"package " & drawVar ia n & drawAlfCtx ia ctx,
	     drawPackageBody b]
	  where
	    vboxlD'' =
	      case details of
	        NamesAndTypes -> mvboxlD ia
		_ -> vboxlD'

            drawPackageBody b =
	      case b of
                PackageDef ds ->
                    k' ia"with " !/
                    {-indentedD-} (drawLetDecls ia ds) !/
                    k' ia "end of package"
		PackageInst e -> eqD & indented (drawExp ia e)

	Open (e,oas) ->
	    k' ia "open " & drawExp ia e & "\\;" !
	    k' ia "use " & drawOpenArgs ia oas
	Data (n,(ctx,sum)) ->
	    k' ia"data " & drawVar ia n & drawAlfCtx ia ctx &
		     eqD & drmath (\ia sum ->embrace (drawData ia sum)) ia sum
	Type (n,(ctx,e)) ->
	    k' ia"type " & drawVar ia n & drawAlfCtx ia ctx &
		     eqD & drawExp ia e
	Axiom (n,(ctx,e)) ->
	    k' ia"postulate " & drawVar ia n & drawAlfCtx ia ctx &
		     elemD & drawExp ia e
        Binding (n,e) -> drawVar ia n & eqD & drawExp (iadata ia n emptyCtx e) e
	Value (en,(ctx@(Ctx l ps),eval,etype)) ->
	    vboxlD' [sigD, {-hvIndentD 10 vindent-} defD]
	  where
	    (hctx,vctx) = apBoth (Ctx l) $ splitAt (sigHiding en) ps
	    sigD = drawVar ia en & drawAlfCtx ia vctx & elemD &
	           drawExp ia etype & "\\qquad" & drawAlfCtx ia hctx
	    defD = drawEqn ia' (c en,(map fst ps,eval))
		   where
		     c (U.Var s) = U.Con s
		     ia' = iadata ia en ctx eval
            vindent = if isND then 30 else 0
	    isND = case eval of EProofOf _ _ -> True; _ -> False
      where
	iadata ia en ctx eval = case stripAnnots eval of
			          ESum _ -> iadata
#ifdef IAGDA
			          EEmptyIndSum -> iadata
#endif
			          _ -> ia
		       where
		         iadata = ia { data_lhs= Just(app (EVar en) args) }
			 args = map EVar (namesCtx ctx)

	(vboxlD',hboxcaD') =
	    case details of
	      JustNames     -> (vboxl1D,hboxc1D)
	      NamesAndTypes -> (vboxl1D,wtD)
	      _             -> (mvboxlD ia,wtD)
	  where
	    vboxl1D = {-vertlD .-} vis1D
	    hboxc1D = {-horizcD .-} vis1D
	    wtD (nameD:ctxD:theRestD) =
		wideOrTallD [hboxcaD [nameD,ctxD],hboxcaD theRestD]
    --}

    drawOpenArgs ia oas = hCommaSepD (map (drawOpenArg ia) oas)

    drawOpenArg ia (OpenArg ps n optt optas) =
        drawProps ps & optasD ! drawVar{-NoSel-} ia n ! opttD
      where
	opttD = case optt of
		  Nothing -> nil
		  Just t -> sep ! elemD & drawExp ia t
	optasD = case optas of
		   Nothing -> nil
		   Just n2 -> drawVar ia n2 & eqD

    drawAlfCtx = drmath (drawAlfCtx' layoutStyle)
    drawAlfCtxOpt = drawAlfCtx' . fromMaybe layoutStyle
    drawAlfCtx' Wide = drawCtx
    drawAlfCtx' Tall = drawCtxTall

    drawCtxTall ia (Ctx l ps@(_:_:_)) =
      paren $ drawTable ia (elemD' ia) (drawExp ia) ps
    drawCtxTall ia ctx = drawCtx ia ctx

    drawCtx ia ctx@(Ctx l []) = nil
    drawCtx ia (Ctx l ps) = paren . drawCtx' ia $ ps

    drawCtx' ia ctx =
	if compactOn
	then hCommaSepD $ map drawBindings $ collectBySnd ctx
	else hCommaSepD (map (drawBinding ia) ctx)
      where
        drawBindings (xs,t) =
	    hboxcaD [hCommaSepD (map (drawVar ia) xs),
	             elemD,drawExp ia t]
     --}
    {-
    drawGoal (Goal g e) = drawMeta ia g & elemD & drawExp ia e
    drawSolution (Solution g e) = drawMeta ia g & eqD & drawExp ia e
    drawConstraint (Constraint e1 e2) = drawExp ia e1 & eqD & drawExp ia e2
    drawAssumption (Assumption var e)
        = drawVar ia var & elemD & drawExp ia e' & hctxD
      where
	(args,res) = flatPi e
	(hargs,vargs) = splitAt (sigHiding var) args
	e' = piExp vargs res
	hctxD = if null hargs
		then nil
		else "\quad " & paren (drawCtx' ia hargs)
    --}
    drawArgs ia assoc prec' = (drawExp' ctxl,drawExp' ctxr)
      where
	ctxl = ctx LHS ia
	ctxr = ctx RHS ia
	ctx = prec . InfixArg assoc prec'

--    drawExps = drawExps' drawExp
    --drawAExps = drawExps' . drawAExp
    --drawExps' draw = hboxcaD . map draw

    drawAExp = mdrawExp' . prec Atomic
    drawBExp = mdrawExp' . precBExp
    drawExp  = mdrawExp' . prec TopLevel
    drawPat ia = drawExp (ia{-{isPattern=True}-})
    prec p ia = ia {syn_ctx=p}
    precBExp = prec (InfixArg NonAssoc maxBound LHS)

    drawExpNoSel' ia = mdrawExp' ia
    --drawExp' = {-propIA-} drawExp''

    mdrawExp' ia =
      --trace ("mdrawExp' "++show ia) $
      drmath drawExp' ia

    drawExp' :: IA -> Exp -> Document
    drawExp' ia e =
      -- "'" (haskell mode font-lock bug workaround)
      --trace (unwords ["drawExp'",show ia,show e]) $
      -- The ia arguments to the a1..a5 function below are meaningless!
      case e of
	EApp e1 e2 -> drawApp e1 e2
        EVar i -> idParen i $ drawEVar i
	EChar c -> k' ia(show c)
	EString s -> k' ia(show s)
	EInt n -> k' ia(show n)
	ERat r -> k' ia(show r)
	ECon c -> noParen $ drawConName ia c
	EAbs nt e2 -> drawAbsExp ia e
	ETyped e t -> drawTypedExp e t --  && hideTrivialLet
	EProofOf t e -> drawProof t e  -- && proofStyle/=UglyProof
	ELet ds body -> drawLetExp ia ds body
	ECase e bs -> drawCase wideOrTallBraceD ia e bs

	EPi _ _ -> drawPiExp e
        ESort sort -> noParen $ drawSort ia sort
	ESum s -> drawSum wideOrTallBraceD ia s

	EProj e n -> drawAExp ia e & "." & drawELabel n
	EOpen e1 oas e2 ->
	  paren'' ia $
	  vboxlD [k' ia"open " & drawExp ia e1 & "\\;" ! k' ia "use " & drawOpenArgs ia oas,
		  k' ia"in " & drawExp ia e2]
	ESig sig -> drawSigExp wideOrTallBraceD ia sig
	EStr str -> drawStruct wideOrTallBraceD ia str
	EMeta meta -> drawMeta ia meta
	EAnnot a@(LayoutDir d) e' ->
	    case e' of
	      EApp e1 e2 -> drawApp' (Just d) ia' e1 e2 -- Error: Out of A-regs
	      --ECon c es  -> drawConApp ia' (Just d) c es
	      ECase e bs -> drawCase wtD ia' e bs
	      EPi _ _    -> drawPiExp' ia' (Just d) e'
	      ESum s     -> drawSum wtD ia' s
	      ESig sig   -> drawSigExp wtD ia' sig
	      EStr str   -> drawStruct wtD ia' str
	      --ECom ix    -> drawTheory wtD ia' ix
	      _ -> drawExp' ia' e'
          where wtD = wideOrTallBraceD' d
	        ia' = {-drfp-} ia
	EAnnot a@(AsTextBy (N fn)) e' ->
	    case lookup fn altdispfs of
	      Just dr -> drawText ia (dr (syn e'))
	      Nothing -> drawExpNoSel' ia e
	EAnnot a@(AltViewsBy (N fns)) e' ->
	    vboxlD (drawExp' ia e':
		    [drawText ia (dr (syn e'))
		     | Just dr<-map (flip lookup altdispfs) fns])
	EAnnot a@(EComment s) e ->
	  vboxlD ["\\alfaexpcomment{"!cmnt s!"}",
		  drawExp' ia e]
	EAnnot a@Hidden e' -> pr "\\ldots"
	{-
	EAnnot a@(InEnv bs) e' ->
	    a2 ia b $ hboxcaD' 1 [drawExp' ia e',drawEnv (smaller ia) bs]
	  where
	    b e bs = EAnnot (InEnv bs) e
	    drawEnv ia =
	        embrace . drawTable ia (eqD' ia) (drawExp ia)

	EAnnot a e | isfold a->
	    paren' ia $ hboxcaD' 1 [symb a,drawBExp ia e]
	  where
	    symb a = s' ia (symb' a)
	    symb' (FoldTo _)   = "¨"
	    symb' (UnfoldTo _) = "·"
	    symb' _ = " " -- shouldn't happen
	    isfold (FoldTo _) = True
	    isfold (UnfoldTo _) = True
	    isfold _ = False
	-}
	EAnnot a e -> drawExpNoSel' ia e
{-
	  where ia' =
		  case a of
		    LayoutDir d -> ia { layoutdir=Just d }
		    _ -> ia
-}
      where

	paren''l = case syn_ctx ia of
		     InfixArg _ _ LHS -> paren
		     _ -> paren' ia

	paren'' ia = case syn_ctx ia of
		    InfixArg _ _ _ -> paren
		    _ -> paren' ia

        paren' ia = case syn_ctx ia of
	           Atomic -> paren
		   _      -> noParen

        idParen x = 
	  if isDistfix (idFixity (lookupArgOpts drawOpts x))
	  then paren' ia
	  else noParen
	    
        noParen = id

	--arrowRhs d = hboxcaD [rparD,rarrowD,d]
	--lambdaBody d = hboxcaD [rarrowD,d]

        --drawConApp ia dir c es = drawFlatApp' dir ia (eCon0 c,es)
	{-
        drawConApp ia dir c@(Con s) es =
	  case (lookupArgOpts drawOpts c,es) of
	    (ArgOptions {idFixity=Infix assoc prec'},[e1,e2]) -> 
	        a3 ia (\e1 c e2-> eCon2 c e1 e2) $ parenOp ia assoc prec' $
		    hboxcaD [drL e1,drawConName ia c,drR e2]
		  where
		    (drL,drR) = drawArgs ia assoc prec'
	    _ -> as ia b $ paren' ia $
	        wideOrTallD' (maxWidth drawOpts) dir
		   (drawConName ia c:map (drawAExp ia) es)
	      where b (n:es) = eCon (out n) (map out es)
	-}

	drawCase drawD ia e bs =
	    drawD (k' ia "case " & drawExp ia e & "\\;" ! k' ia "of") (drawBranches ia bs)

	drawSum drawD ia s = drawD (k' ia "data") (drawData ia' s)
	  where ia' = ia --{ layoutdir=Nothing }

	drawSigExp drawD ia sig = drawD (k' ia"sig ") (drawSig sig)
	  where
	    ia' = ia --{ layoutdir=Nothing }
	    drawSig = vboxlrD . map drawSigPart

	    drawSigPart sp =
	      case sp of
	        SigField lbl e -> drawLabel ia lbl & elemD &drawExp ia' e
		SigDef defb ->drawDefB ia Nothing defb

	drawStruct drawD ia str =
	    drawD (k' ia"struct") $ drawLetDecls ia str
	  where ia' = ia --{ layoutdir=Nothing }

	wideOrTallBraceD = wideOrTallBraceD' layoutStyle

	wideOrTallBraceD' d =
	    case d of
	      Tall -> tallD
	      Wide -> wideD
	  where
	    wideD preD subD = paren' ia $ hboxcD [preD, embrace subD]
	    tallD preD subD = paren'' ia $ vboxlD [preD,"\\;\\;" & indented subD]

	drawPiExp = drawPiExp' ia Nothing
	drawPiExp' ia dir e@(EPi (p1:-e2) e3) =
	  if p1==noname
	  then drawArrow ia e2 e3
	  else if compactOn && p1/=noname
	       then drawPiExpCompact dir ia (flatPi' e)
	       else drawPiExpExpanded ia p1 e2 e3

	drawPiExpCompact dir ia (actx,e) =
	    wideOrTallD' (maxWidth drawOpts) dir
		 [paren (drawCtx' ia ctx) & rarrowD,
		  drawExp' ia e]
	  where (as,ctx) = unzip actx

        drawPiExpExpanded ia p1 e1 e =
	    paren'' ia $
	    hboxcaD [paren (hboxcaD [drawVar ia p1,elemD,drawExp ia e1]),
		     rarrowD,drawExp' ia e]
	  where ePi n t e = EPi (n:-t) e

        drawArrow ia e1 e2 =
	    paren'' ia $
	    hboxcaD [drawBExp ia e1,rarrowD,drawExp ia e2]

        drawAbsExp ia e@(EAbs nt e2) =
	  case syn_ctx ia of
	    Subgoal -> drawHyp ia (flatAbs' e)
	    _ -> if compactOn
		 then drawAbsExpCompact ia (flatAbs' e) -- handle annotations!!!
		 else drawAbsExpPlain ia nt e2

        drawHyp ia (ants,e) =
	    placeD [hypsD,drawExp ia e]
          where
	    ia' = smaller ia
	    hypsD = embrack $ vboxlD $ map (drawTyping ia') nts
	    (as,nts) = unzip ants
	    placeD = case proofStyle drawOpts of
		       BrorProof -> vboxlD
		       _ -> hboxcaD

        drawAbsExpCompact ia (ants,e) =
	    paren'' ia $
	    hboxcaD [lambdaD,drawHiddenTypings ia nts,rarrowD,
	             drawExp' (prec TopLevel ia) e]
	  where
	    (as,nts) = unzip ants

        drawAbsExpPlain ia nt e =
	  paren'' ia $
          hboxcaD [lambdaD,drawTyping ia nt,rarrowD,
	           drawExp' (prec TopLevel ia) e]

        drawEVar n@(U.Var s) = drawName' ia (DO.Var s)
        drawELabel n@(Label s) = drawName' ia (DO.Name s)

	drawApp = drawApp' Nothing ia
	drawApp' dir ia e1 e2 = drawFlatApp' dir ia (flatApp e1 e2)
	drawFlatApp' dir ia (f,args) =
	    plainApp ia $
	    case (fixity,map splitAnnots visargs) of
	      (Postfix prec',[arg]) ->
		(drawPostfix prec' fhid (aa arg),[])
	      (Infix assoc prec',[e1,e2]) ->
		(drawInfix2 assoc prec' fhid (aa e1) (aa e2),[])
	      (Infix assoc prec',[e1]) ->
		(drawInfix1 assoc prec' fhid (aa e1),[])
	      (Quantifier sd,[(a,EAbs n e20)]) ->
		(drawQuantifierAbs sd fhid a n e20,[])
	      (Quantifier sd,[e2]) ->
		(drawQuantifier sd fhid (aa e2),[])
	      {-
	      (Big,[e1,e2,(a,EAbs nt e3)]) ->
		(drawBigAbs fhid (aa e1) (aa e2) a nt e3,[])
	      (Big,[e1,e2,e3]) ->
		(drawBig fhid (aa e1) (aa e2) (aa e3),[])
	      (Distfix3 assoc prec',[e1,e2,e3]) ->
		(drawDistfix3 assoc prec' fhid (aa e1) (aa e2) (aa e3),[])
	      (Distfix3b assoc prec',[e1,e2,e3]) ->
		(drawDistfix3b assoc prec' fhid (aa e1) (aa e2) (aa e3),[])
	      (Distfix4 assoc prec',[e1,e2,e3,e4]) ->
		(drawDistfix4 assoc prec' fhid (aa e1) (aa e2) (aa e3) (aa e4),[])
	      -}
	      (Tuple,es) -> (drawTuple fhid visargs,[])
	      (Fraction,[e1,e2]) -> (drawFraction fhid (aa e1) (aa e2),[])
	      (Distfix assoc prec',es) -> drawDistfix assoc prec' fhid visargs viscnt
	      (ProofGoal,[e1,e2]) | null hidargs -> (drawProofGoal fhid (aa e1) (aa e2),[])
	      (_,args) -> (hiddenApp fhid,map aa args)
	  where
	    aa (a,e) = attachAnnots a e
	    fhid = (f,hidargs)
	    (hidargs,visargs) = splitAt hide args
	    hide = if hidingOn then hidcnt else 0
	    argcnt = length args	-- implies argcnt>=1
	    viscnt = argcnt-hide	-- not hidingOn implies viscnt>=1
	    --isinfix = isInfix fixity
	    (hidcnt,fixity) =
	        case f of
		  EVar x -> varOptions x
		  ECon c -> conOptions c
		  _ -> (0,Nonfix)
	    small = ia {{- font_size=sz',-} syn_ctx=TopLevel } -- !!!
	    big   = ia { {-font_size=sz+3,-} syn_ctx=Atomic } -- !!!
	    c = hCenterD -- spacer is lost when replacing a subexpr!!

	    plainApp ia (d,[]) = d ia
	    plainApp ia (d,args) =
	        paren' ia $
		wideOrTallD' (maxWidth drawOpts) dir
			(d ia':map (drawAExp ia) args)
	      where ia' = precBExp ia

	    hiddenAExp fhid ia = hiddenApp fhid (prec Atomic ia)
	    hiddenBExp fhid ia = hiddenApp fhid (precBExp ia)

	    hiddenApp (f,_) ia = drawExp' ia f

	    drawTuple fhid es ia =
		emabrack $ hCommaSepD (map (drawExp ia) es)

            drawFraction fhid e1 e2 ia =
	      "\\frac{" & drawExp ia e1 & "}{" & drawExp ia e2 & "}"

            drawProofGoal (f,hargs) e1 e2 ia = drawProof e1 e2

	    -- pre: viscnt == length visargs
	    drawDistfix assoc prec' fhid@(f,hidargs) visargs viscnt =
	        case (apFst groupArgs . nameOpts) `fmap` funname f of
		  Just (ss,bmsrc) | argcnt == viscnt ->
		      (\ia->parenOp ia assoc prec' $ hboxcaD $
		       dist1D (mix ss (permute visargs p)),
		       [])
	            where
	              argcnt = mixfixArity p
		      p = permutation ss
		      (drL,drR) = drawArgs ia assoc prec'
		      dist1D (Right e:es) = drL e:distD es
		      dist1D es = distD es
                      distD [] = []
	              distD [Right e] = [drR e]
	              distD (Right e:es) = drawExp ia e:distD es
		      distD (Left s:es) = dw s:distD es
		      dw = wmap prw . groupBy sp
                         where
			   sp c1 c2 = isSpace c1 == isSpace c2
			   prw w@(c:_) = if isSpace c
				 	 then pr (concatMap (const "\\;") w)
					 else drawName''' d' s' ia (w,bmsrc)
		  _ -> (hiddenApp fhid,visargs)
	      where funname (EVar v) = Just (DO.name v)
		    funname (ECon c) = Just (DO.name c)
		    funname _ = Nothing

	    drawPostfix prec' e1 e2 ia =
	        parenOp ia NonAssoc prec' $ drL e2 & hiddenBExp e1 ia
	      where (drL,_) = drawArgs ia Assoc prec'

	    drawInfix1 assoc prec' e1 e2 ia =
		paren $ drL e2 & hiddenBExp e1 ia
	      where (drL,_) = drawArgs ia assoc prec'

	    drawInfix2 assoc prec' op e1 e2 ia =
		parenOp ia assoc prec' $
		--wideOrTallD' (maxWidth drawOpts) dir
		  hboxcaD [drL e1, hiddenBExp op ia, drR e2]
	      where (drL,drR) = drawArgs ia assoc prec'
	    {-
	    drawDistfix4 assoc prec' op e1 e2 e3 e4 ia =
		a5 ia d4app $
		parenOp ia assoc prec' $
		tableD' 1 3 [bl,noRefsD $ c $ drawExp' small e1,bl,
			     drL e3,c $ hPadD 4 $ hiddenBExp op ia,drR e4,
			     bl,noRefsD $ c $ drawExp' small e2]
	      where
	        (drL,drR) = drawArgs ia assoc prec'
		d4app e1 e3 op e4 e2 = EApp (app op [e1,e2,e3]) e4

	    drawDistfix3 assoc prec' op e1 e2 e3 ia =
		a4 ia d3app $
		parenOp ia assoc prec' $
		tableD' 1 3 [blankD 2,noRefsD $ c $ drawExp' small e1,blankD 2,
			     drL e2,c $ hPadD 4 $ hiddenBExp op ia,drR e3]
	      where
	        (drL,drR) = drawArgs ia assoc prec'
		d3app e1 e2 op e3  = EApp (app op [e1,e2]) e3

	    drawDistfix3b assoc prec' op e1 e2 e3 ia =
		a4 ia d3app $
		parenOp ia assoc prec' $
		tableD' 0 3 [drL e2,c $ hPadD 4 $ hiddenBExp op ia,drR e3,
		             blankD 2,noRefsD $ c $ drawExp' small e1]
	      where
	        (drL,drR) = drawArgs ia assoc prec'
		d3app e2 op e3 e1 = EApp (app op [e1,e2]) e3
	    -}
	    drawQuantifierAbs sd q a nt e ia =
	        qParen sd $
	        hboxcaD [hiddenAExp q ia,
			 drawTyping'' (not sd) ia nt,
			 qBodyD sd (drawExp ia e)]

	    drawQuantifier sd q e ia =
	        qParen False $
	        hboxcaD [hiddenAExp q ia,
			 drawName' ia n,
			 qBodyD False (hboxcaD [drawBExp ia e,drawName' ia n])]
	      where n = DO.Var "x" -- !! hmm, should pick an unused variable
		    -- !! Confusing to show a variable that doesn't exist!

            qParen sd = if sd || quantifierDots
			then paren'' ia
			else paren' ia
            qBodyD sd d =
	      if sd || quantifierDots
	      then k' ia"." & d
	      else paren d
	    {-
	    drawBigAbs bigop e112 e12 a nt@(U.Var s:- _) e2 ia =
		a4 ia bApp $
		paren''l $
		tableD' 1 2
		  [noRefsD $ c $ drawExp' small e12,blankD 2,
		   c $ hiddenApp bigop big,vCenterD $ drawExp ia e2,
		   noRefsD $ c $ hboxcaD' 2 [drawName' sz' (DO.Var s),eqD' sz',drawExp' small e112]]
	      where
		bApp e12 big e2 e112 =
		  EApp (EApp (EApp big e112) e12) (aa (a,EAbs nt e2))

	    drawBig bigop e112 e12 e2 ia =
		a4 ia bApp $
		paren''l $
		tableD' 1 2
		  [noRefsD $ c $ drawExp' small e12,blankD 2,
		   c $ hiddenApp bigop big,
		   noRefsD $ vCenterD $ drawExp ia e2,
		   noRefsD $ c $ drawExp' small e112]
	      where
		bApp e12 big e2 e112 = EApp (EApp (EApp big e112) e12) e2
            -}
	parenOp ia assoc prec =
	  if needParen (syn_ctx ia) assoc prec
	  then paren
	  else noParen

        drawLetExp ia ds body =
	  paren'' ia $
	  vboxlD [hboxcaD [k' ia"let",drawLetDecls ia ds],
		  hboxcaD [k' ia"in",drawExp ia body]]

        drawTypedExp = drawTypedExp' ETyped
        drawTypedExp' bld e t =
	  if hideTrivialLet drawOpts
	  then drawExp ia e
	  else case e of
	         EMeta _ ->
		      paren'' ia $
		      "{"!drawExp ia e!"}_{"!drawExp ia t!"}"
		 _ -> paren'' ia $
		      vboxlD [embrack $ drawExp ia t,drawExp ia e]

        drawProof t e = 
	  case proofStyle drawOpts of
	    NDProof   -> drawNDProof e' t
	    --BrorProof -> drawBrorProof e' t
	    _ -> drawTypedExp' (flip EProofOf) e t
	  where
	    e' = splitAnnots e
	    ndgoal = ndgoalVar

	    ruleNameD rule hidden =
	       -- Trying to make both math mode and paragraph mode text smaller
	       "\\small{\\scriptstyle{" !
	       drawExp' ia' (app rule hidden)
	       ! "}}"
	      where ia' = prec (NDGoal ()) (smaller ia)

            drawSubgoal = drawExp' (prec Subgoal ia)

	    drawNDProof (annots,e) t =
	      case flat e of
		(rule,subgoals) ->
		    noParen $
		    hboxcaD' 3 [ruleD, ruleNameD rule hidden]
		  where
		    ruleD =
		      "\\cfrac{" ! subgoalsD ! "}{" ! drawExp ia t ! "}"

		    subgoalsD =
			prsep "\\quad " (map drawSubgoal visible)

		    (hidden,visible) = splitAt hidecnt subgoals
		      where
			hidecnt =
			  case rule of
			    EVar x {-| hidingOn-} ->
				hideCnt (lookupArgOpts drawOpts x)
			    _ -> 0

	    {-
	    drawBrorProof (annots,e) t =
		case flat e of
		  (rule@(EVar rulename),subgoals) ->
		      a3 ia b $ noParen $
		      vboxlD [drawExp ia t,
			      hboxD [vLineD, vboxD [ruleNameD rule hidden,
					            drawSubgoals]]]
		    where
		      b t rule es = eProofOf annots t (app rule es)
		      drawSubgoals = hCenterD $ vboxlD' 15 $
		                     map drawSubgoal visible
		      drawSubgoal subg =
			    hboxD' 0 [spacedD topS (s "¾"),drawExp ia subg]
		      (hidden,visible) = splitAt hidecnt (subgoals)
		      hidecnt = if hidingOn
				then hideCnt (lookupArgOpts drawOpts rulename)
				else 0
		  _ -> a2 ia EProofOf $ noParen $
		       vboxlD [drawExp ia t,drawExp ia e]
	    -}

	flat (EApp e1 e2) = flatApp e1 e2
	flat e = (e,[])
	eProofOf annots t e = EProofOf t (attachAnnots annots e)
   
    drawMeta ia meta =
	if meta<0
    	then pr "?"
 	else "?_{" ! show (meta::Int)!"}"

    drawData ia = drawConstrs
      where
	drawConstrs = vboxlD . map drawConstr
	drawConstr (Constr (n,(ctx,restrs))) =
	    drawConNameSel ia n & drawAlfCtx ia ctx & drawRestrs restrs

        drawRestrs [] = nil
	drawRestrs es =
	    case data_lhs ia of
	      Just lhs -> hboxcaD [elemD,drawExp ia (app lhs es)]
	        where
	      _ -> elemD & hboxcaD (pr "\\verb!_!" :map (drawAExp ia) es)
                   -- hboxcaD (elemD:k' ia "_":map (drawAExp ia) es)

    drawBranches ia = vboxlD . map (drawBranch ia)
      where drawBranch = drawBranch' drawPat rarrowD
    drawEqn ia = mvboxlD ia . (:[]) . drawBranch' drawPat' eqD ia . Branch
      where drawPat' ia e =
              case flatApp' e of
	        (ECon (U.Con n), es) -> drawPat ia (app (EVar (U.Var n)) es)

    drawBranch' patD sepD ia = 
	--trace (show ia) $
	if compactOn
	then drawBranchCompact
	else drawBranchPlain
      where
        -- !! Experimental !!
        drawBranchCompact b = 
	    drawMatch (compactBranch b)
	  where
	    drawMatch (lhs@(con,vars),match) =
	      case match of
	        Simple (pat,e) ->
		    wideOrTallD [hboxcaD [patD ia pat,sepD],
				 drawExp ia e]
		Nested f x ms -> msD
		  where
		    msD = if null ms
		          then nil
			  else mvboxD ia (map drawMatch ms)
        --}

	drawBranchPlain (Branch (con{-@(U.Con s)-},(vars,e))) =
	    hboxcaD [patD ia (eCon con (map EVar vars)),sepD,drawExp ia e]
    --}
    wideOrTallD = wideOrTallD' (maxWidth drawOpts) Nothing

    drawBinding ia = drawBinding' (drawVar ia) (elemD' ia) (drawExp ia)


    drawTable ia = drawTable' (drawVar ia)
    drawTable' lhs sep rhs = vboxlrD . map (drawBinding' lhs sep rhs)
    drawBinding' lhs sep rhs (n,e) = lhs n & sep & rhs e

    drawConName ia n = drawConNameSel ia n

    drawConNameSel ia n@(U.Con s) = drawName'' c' s' ia (DO.Con s)

    drawTyping' = drawTyping'' compactOn
    drawTyping'' b = if b then drawHiddenTyping else drawTyping

    drawTyping ia (n:-t) =
        hboxcaD [drawVar ia n,elemD,drawExp ia t]

    drawHiddenTypings ia = hboxcaD . map (drawHiddenTyping ia)
    drawHiddenTyping ia (n:-t) = drawVarNoSel ia n

    drawVarNoSel ia n = drawVar ia n

    drawVars ia = hboxcaD . map (drawVar ia)
    drawVar ia n@(U.Var s) = drawName' ia (DO.Var s)
    drawCon ia n@(U.Con s) = drawName'' c' s' ia (DO.Con s)
    drawLabel ia n@(Label s) = drawName' ia (DO.Name s)

    drawName' = drawName'' v' s'
    drawName'' n s ia =  drawName''' n s ia . nameOpts
    drawName''' normal symbol ia (dstr,bmsrc) = i dstr
      where
	i = case bmsrc of
	      UseNormalFont -> normal ia
	      UseSymbolFont -> symbol ia
	      UseImageFile -> g {- . BitmapFile -} -- !!

    nameOpts name =
	(fromMaybe (nameStr name) (displayAs opts),bitmapSource opts)
      where opts = lookupArgOpts drawOpts name
	
    --drawSort n@(Sort s) = annotSa (const (SortS n)) $ v s
    drawSort ia n@(Sort s) = v' ia s

    drawText ia = mparbox ia . drawParas
      where
        ia' = ia{math_mode=False}

        drawParas = prsep (nl!nl) . map drawPara

        drawPara (PlainPara ws) = wmap drawTextItem ws
	drawPara (NestedPara txt) =
	    environment env (indented (drawParas txt))
	  where
	    env = if bulleted txt
		  then "itemize"
		  else "quote"

            bulleted (PlainPara []:ps) = bulleted ps
	    bulleted (PlainPara (TPlain "·":_):_) = True
            bulleted txt = trace ("Not bulleted: "++take 160 (show txt))
			   False

	drawTextItem ti =
	  --trace (show ti) $
	  case ti of
	    TSyntax stx -> draw drawOpts ia'  altdispfs stx
	    TPlain "·" -> g "\\item" -- hmm
	    TPlain s -> g s
	    TSymbol Nothing symb -> s' ia' symb
	    TSymbol (Just font) symb -> {-fontD font-} (g symb)
	    TVar s -> v' ia' s

    sigHiding var =
      if hidingInSignatures && hidingOn
      then fst (varOptions var)
      else 0

    varOptions (U.Var s) = idOptions (DO.Var s)
    conOptions (U.Con s) = idOptions (DO.Con s)

    idOptions name =
      case lookupArgOpts drawOpts name of
        ArgOptions {hideCnt=hideCnt, idFixity=idFixity} -> (hideCnt,idFixity)

    --iaTop' = iaTop drawOpts

    smaller ia = ia -- { font_size=font_size ia - 1} -- !!

    hCommaSepD = hpr . intersperse (d", ")
    --commaSepD = intersperse (d", ")
    --commaD = d ","

    --wk = west.k

    v',k',d'::IA->String->Document
    k' ia = mmbox ia .  ("{\\bf"&).(!"}"). escape
    c' ia = k' ia -- pr . escape
    v' ia = mmbox ia . ("{\\it"&).(!"}") . escape
    s' ia = mmath ia . pr . symescape
    d' s = mmbox ia . pr . escape
    --ds' s = d' s
    ds' s = pr . escape
    cmnt = cmntD.g.escape
      where
        --cmntD d = "{\\em"&d!"}"
        cmntD d = d

    --v,k,d::String->Document
    --v = v' ia
    --k = k' ia
    --s = s' ia
    --ds = ds' ia
    d = d' ia
    --ds = ds' ia
    --l = fontG litFont.g

--wideOrTallD' maxwidth dir = prsep "\\;" -- !!!
wideOrTallD' maxwidth dir =
    case dir of
      Just Wide -> hboxcaD
      Just Tall -> tallD
      Nothing -> autoP
  where
    tallD (d:ds) = vboxlD [d,vboxlD ds] -- indent all but the first

    autoP = hboxcaD -- !!
{-
    autoP = ifSizeP p wideP tallP
      where p (Point w1 _) (Point w2 _) =
              -- "'" (haskell mode font-lock bug workaround)
	      --trace (show ("wideOrTallD'",w1,w2,maxwidth)) $
	      w1<=maxwidth || w1-w2<150 && w2>9*w1 `div` 10
-}

vis1D [] = trace "vis1D []" nil
vis1D (d:ds) = d

interleave x [] = [x]
interleave x (y:ys) = x:y:interleave x ys

--- New stuff for LaTex:

drmath :: (IA->a->Document) -> IA -> a -> Document
drmath f ia = mmath ia . f ia{math_mode=True}

mmbox,mmath,mparbox::IA->Document->Document
mmbox = ifmath mbox id 
mmath = ifmath id math
mparbox = ifmath parbox id

ifmath t e ia = if math_mode ia then t else e
emath d = "\\ensuremath{"!d!"}"

small d = "{\\small"&d!"}"
mbox d = "\\mbox{"!d!"}"
math d = "\\("!d!"\\)"
parbox d = "\\parbox{12cm}{"!d!"}"
vboxlD ds = "\\begin{array}{l}" !/ arows ds !/ "\\end{array}" 
  where arows = vpr . intersperse (pr "\\\\"!nl)
vboxlrD = vboxlD

mvboxlD ia ds =
  if math_mode ia
  then vboxlD ds
  else environment "flushleft" $ vmap (!nl) ds

mvboxD ia ds =
  if math_mode ia
  then vboxlD ds
  else vmap (!nl) ds

eqD = emath "\\equiv"
elemD = emath "\\in"
elemD' _ = elemD
rarrowD = pr "\\rightarrow"
lambdaD = pr "\\lambda"

embrace = delim "\\{" "\\}"
paren = delim "(" ")"
embrack = delim "[" "]"
emabrack = delim "\\langle" "\\rangle"

delim l r d = "\\left" & l & d & "\\right" & r

indentedD d = environment "quote" d -- hmm !!
environment env d = cmd1 "begin" env !/ d !/ cmd1 "end" env

cmd1 cmd arg = "\\"++cmd++"{"++arg++"}"

emaths = cmd1 "ensuremath"

hboxcaD' sep = hboxcaD
hboxcaD ds = hboxcD ds
hboxcD ds = prsep "\\," ds
--hboxcD ds = wpr ds -- !!
hCenterD = id -- !!

g x = pr x

escape = concatMap escape1
  where
--    escape1 '\'' = "^\\prime" -- But ' should work! must be in math mode!!
    escape1 '~' = emaths "\\sim"
    escape1 '<' = emaths "<"
    escape1 '>' = emaths ">"
    escape1 '·' = emaths "\\bullet" -- or \cdot
    --escape1 '·' = "\\item" -- hmm, assumes special use of ·
    escape1 c = if c `elem` special
		then ['\\',c]
		else [c]

special = "#$%&~_^\\{}"

symescape = concatMap escape1
  where
    escape1 '<' = "\\ensuremath{<}"
    escape1 '>' = "\\ensuremath{>}"
    escape1 '·' = "\\ensuremath{\\bullet}" -- or \cdot
    --escape1 '·' = "\\item" -- hmm, assumes special use of ·
    escape1 '\"' = "\\forall " -- '\"'
    escape1 '$' = "\\exists "
    escape1 '\'' = "\\ni "
    escape1 '@' = "\\cong "
    escape1 'Ú' = "\\vee "
    escape1 'Ù' = "\\wedge "  
    escape1 'Ø' = "\\neg "
    escape1 '^' = "\\bot "
    escape1 '£' = "\\leq "
    escape1 '¥' = "\\infty "
    escape1 '§' = "\\clubsuit "
    escape1 '¨' = "\\doamonsuit "
    escape1 '©' = "\\heartsuit "
    escape1 'ª' = "\\spadesuit "
    escape1 '«' = "\\leftrightarrow "
    escape1 '¬' = "\\leftarrow "
    escape1 '­' = "\\uparrow "
    escape1 '®' = "\\rightarrow "
    escape1 '¯' = "\\downarrow "
    escape1 'Û' = "\\Leftrightarrow "
    escape1 'Ü' = "\\Leftarrow "
    escape1 'Ý' = "\\Uparrow "
    escape1 'Þ' = "\\Rightarrow "
    escape1 'ß' = "\\Downarrow "
    escape1 '±' = "\\pm "
    escape1 '³' = "\\geq "
    escape1 '´' = "\\times "
    escape1 'µ' = "\\propto "
    escape1 '¸' = "\\div "
    escape1 '¹' = "\\neq "
    escape1 'º' = "\\equiv "
    escape1 '»' = "\\approx "
    escape1 '¼' = "\\ldots "
    escape1 'Ä' = "\\otimes "
    escape1 'Å' = "\\oplus "
    escape1 'Æ' = "\\emptyset "
    escape1 'Ç' = "\\cap "
    escape1 'È' = "\\cup "
    escape1 'É' = "\\supset "
    escape1 'Ê' = "\\supseteq "
    escape1 'Ì' = "\\subset "
    escape1 'Í' = "\\subseteq "
    escape1 'Î' = "\\in "
    escape1 'Ñ' = "\\nabla "
    escape1 'Õ' = "\\prod "
    escape1 '×' = "\\cdotp "
    escape1 'å' = "\\sum "
    ---
    escape1 'a' = "\\alpha "
    escape1 'b' = "\\beta "
    escape1 'c' = "\\chi "
    escape1 'd' = "\\delta "
    escape1 'e' = "\\epsilon "
    escape1 'f' = "\\phi "
    escape1 'g' = "\\gamma "
    escape1 'h' = "\\eta "
    escape1 'i' = "\\iota "
    escape1 'j' = "\\varphi "
    escape1 'k' = "\\kappa"
    escape1 'l' = "\\lambda "
    escape1 'm' = "\\mu "
    escape1 'n' = "\\nu "
    escape1 'p' = "\\pi "
    escape1 'q' = "\\theta "
    escape1 'r' = "\\rho "
    escape1 's' = "\\sigma "
    escape1 't' = "\\tau "
    escape1 'u' = "\\upsilon "
    escape1 'v' = "\\varpi "
    escape1 'w' = "\\omega "
    escape1 'x' = "\\xi "
    escape1 'y' = "\\psi "
    escape1 'z' = "\\zeta "
    escape1 'C' = "X"
    escape1 'D' = "\\Delta "
    escape1 'F' = "\\Phi "
    escape1 'G' = "\\Gamma "
    escape1 'J' = "\\vartheta "
    escape1 'L' = "\\Lambda "
    escape1 'P' = "\\Pi "
    escape1 'Q' = "\\Theta "
    escape1 'R' = "P"
    escape1 'S' = "\\Sigma "
    escape1 'U' = "Y"
    escape1 'V' = "\\varsigma "
    escape1 'W' = "\\Omega "
    escape1 'X' = "\\Xi "
    escape1 'Y' = "\\Psi "
    ---
--    escape1 '\'' = "^\\prime" -- But ' should work! must be in math mode!!
    escape1 '~' = "\\sim" -- must be in math mode!!
    escape1 c = if c `elem` special
		then ['\\',c]
		else [c]
