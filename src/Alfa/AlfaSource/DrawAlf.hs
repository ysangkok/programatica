{-# COMPILERFLAGS -fno-overload-restr #-}
module DrawAlf(drawSyntax,drawTopSyntax,AlfAnnot,inherited,InheritedAttribute(..),hboxcaD) where
import Fud
import DrawUtils
--import NDPlacer(ndP')
--import HorizontalAlignP
import Annot
import EditAnnot
import UAbstract
import qualified UAbstract as U -- because of GHC 4.06 bug
import UAnnots
import AlfAbstract
import AlfaText
import AbstractOps
--import AlfModules
import AlfSyntax
import Fonts
import DrawOptions hiding (Name) -- GHC 4.06 forgets the hiding, it seems
import qualified DrawOptions as DO
import Fixity
import Mixfix
import Patterns
import Collect(collectBySnd)
import Utils2(apBoth)
import ListUtil(assoc,chopList)
import List(intersperse)
import Maybe(fromMaybe)
import HO(apFst)

-- debugging:
import Debug2(trace)
--import Show(show_exp)

#ifdef __HASKELL98__
#define map fmap
#endif

quantifierDots = argFlag "quantifierdots" False
hidingInSignatures = argFlag "hiding_in_signatures" True

data AlfAnnot
  = AlfAnnot { redrawFromParent',
	       okRedrawPoint', -- ok to redraw from here?
	       is_selectable,
	       is_hole :: Bool,
	       inherited :: InheritedAttribute }
  deriving (Show)

alfAnnot' = AlfAnnot False True

--type AlfConstructorArgs = (Context,EqRestr)

type AlfDrawing = ADrawing AlfAnnot Syntax

type IA = InheritedAttribute
data InheritedAttribute
  = IA { syn_ctx :: SyntacticContext (Maybe Exp),
         checked :: Bool,
	 isPattern :: Bool,
	 --isCon :: Bool,
	 font_size :: Int,
	 data_lhs :: Maybe Exp
	 --layoutdir :: Maybe LayoutDir
	}
  deriving (Show)

iaTop drawOpts = IA TopLevel True False {-False-} (fontSize drawOpts) Nothing --False True

--nodrfp ia = ia{redrawFromParent'=False}
--okdr ia = ia{okRedrawPoint'=True}
--drfp ia = ia{redrawFromParent'=True}
--notokdr ia = ia{okRedrawPoint'=False}

instance EditAnnot AlfAnnot where
  isSelectable = is_selectable
  isHole = is_hole
  redrawFromParent = redrawFromParent'
  okRedrawPoint a = isSelectable a && okRedrawPoint' a

drawTopSyntax altdispfs options = drawSyntax' altdispfs options (iaTop options)

drawSyntax altdispfs options a s = seta $ draw options (inherited a) altdispfs s
  where seta = changeAnnot $ \ a0 ->
	       a0{redrawFromParent'=redrawFromParent' a,
		  okRedrawPoint'=okRedrawPoint' a}

drawSyntax' altdispfs options ia s = draw options ia altdispfs (syn s)

{-
drawSyntax     options = fst.draw options
drawGoal       options = fst$fst$fst$snd$draw (wide options) (iaTop options)
drawSolution   options = snd$fst$fst$snd$draw (wide options) (iaTop options)
drawConstraint options = snd$fst$snd$draw (wide options) (iaTop options)
drawAssumption options = snd$snd$draw (wide options) (iaTop options)
-}
wide options = options { maxWidth=16000 }

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
	ExpsS es -> drawAExps ia es
	--CDeclS d -> drawCDecl d
	--CDeclsS ds -> drawCDecls ds
	DeclS d -> drawDecl ia d
	DeclsS ds -> drawDecls ds
	DefBS defb -> drawDefBstd ia declDetail defb
	ImportS i -> drawImport i
	--ImportsS is -> drawImports is
	ModuleS m -> drawModule m
	--BranchS b -> draw
	BranchesS bs -> drawBranches ia bs
	BranchS b -> drawBranch ia b
	ContextS ctx@(Ctx optlayout _) -> drawAlfCtxOpt optlayout ia ctx
	--ComS com -> drawCom ia com
	VarS var -> drawVar sz var
	ConS con -> drawCon sz con
	LabelS label -> drawLabel sz label
	SortS sort -> drawSort sort
	HoleS -> decHole
	TypingS t ->  drawTyping' ia t
	GoalS g -> drawGoal g
	SolutionS s -> drawSolution s
	ConstraintS c -> drawConstraint c
	AssumptionS a -> drawAssumption a
	_ -> -- Prevent prog from crashing, just in case...
	     trace ("drawSyntax: "++typeof s) $
	     a0b s $ d "..."
  where
    drawModule (Module decls) =
      a1 iaTop' Module $ spacedD topS $ drawDecls decls

    --drawImports [] = a0top (ImportsS []) (blankD 5)
    --drawImports is = as0 ImportsS . vboxlD . map drawImport $ is
    drawImport i@(Import filename) =
      a0top i $ hboxcaD [k "import",v filename]

    decHole = a0top HoleS $ g $ flex'' (const [])

    drawComment' s = vboxlD' 0 [cmnt l | l<-lines ("  "++s)]

    drawDecls = as0 DeclsS . drawDecls' ia
    drawDecls' ia = vboxD' 0 . interleave decHole . map (drawDecl ia)

    drawLetDecls ia = as0 DeclsS .
		      drawDecls' ia
    {-
    -- fix to allow pasting of CDecls in let expressions.
      where
	strip = concatMap strip1
	strip1 (DeclS d) = [d]
	strip1 (DeclsS ds) = ds
	strip1 (CDeclsS cds) = stripCDecls cds
	strip1 (CDeclS (Decl' _ d)) = [d]
	strip1 (CDeclS (Comment' _)) = [] -- comments are lost
    -}

    drawDecl ia cd@(Comment s) = a0top cd $ drawComment' s
    drawDecl ia cd@(ImportDecl imp) = a1b ImportDecl $ drawImport imp
    drawDecl ia (Decl checked' defs) =
        annotS ia' (syn. Decl checked' . map out) $
	refEdgesD $
        hboxD (errBar++[lbrackD' d, vboxlD (map (drawDef ia') defs)])
      where
        ia' = ia { checked = checked' && checked ia }
	errBar =
	  if checked' then []
	  else [fgD ["Red","Black"] $ g $ vFiller 5]

    --drawDef :: (Name,(Context,Exp,Exp))-> Drawing (Annot x y) G
    drawDef ia (DefA ps@(_,props,optDetails) def) =
        if null props
        then drawDefB' def
        else vboxlD' 1 [drawProps props,drawDefB' def]
      where
	drawDefB' = a1b (DefA ps) . drawDefB ia optDetails 

    drawProps = hboxcaD . map drawProp
      where drawProp = k' (font_size ia-1) . show

    drawDefB ia optDetails def =
	case details of
	  DeclAsTextBy fn ->
	    case lookup fn altdispfs of
	      Just dr ->
	         noSel $ setNotOkDr $ a2 ia b $
		 vis1D [drawText ia (dr (syn def)),std]
	        where
	          b :: Exps -> DefB -> DefB
		  b _ defb = defb
	      _ -> std
	  DeclAltViewsBy fns ->
	      noSel $ as ia b $
	      vboxlD (std:
		      [drawText ia (dr (syn def))
		       | Just dr<-map (flip lookup altdispfs) fns])
	    where
	      b :: [DefB] -> DefB
	      b (d:_) = d
	  _ -> std
      where
	std = drawDefBstd ia details def
        details = fromMaybe declDetail optDetails

    drawDefBstd ia details def =
      case def of
        CommentDef s -> a0top (defB $ def) $ drawComment' s
        Package (n,(ctx,b)) ->
 	  a3b bP $
	  vboxlD'' [
	     hboxcaD [k "package",
		      hboxcaD' [drawVar sz n,drawAlfCtx ia ctx]],
	     drawPackageBody b]
	  where
	    vboxlD'' =
	      case details of
	        NamesAndTypes -> vboxlD
		_ -> vboxlD'

	    bP n ctx e = defB $ Package (n,(ctx,e))

            drawPackageBody b =
	      case b of
                PackageDef ds ->
	          a1b PackageDef $
		  hboxcaD [k "with",drawLetDecls ia ds]
		PackageInst e ->
		  a1b PackageInst $
		  hboxcaD [eqD,drawExp ia e]

	Open (e,oas) ->
	    --trace (show def) $
 	    a2b b $
	    hboxcaD [k "open",drawExp ia e,k "use",drawOpenArgs ia oas]
	  where b e oas = defB $ Open (e,oas)
	Data (n,(ctx,sum)) ->
	    a3b bD $
	    hboxcaD [k "data",drawVar sz n,drawAlfCtx ia ctx,
		     eqD, refMiddleD $ embrace'' ia $ drawData ia sum]
	  where bD n ctx sum = defB $ Data (n,(ctx,sum))
	        --b (ConstructorsS cs) = cs
	Type (n,(ctx,e)) ->
	    a3b b $
	    hboxcaD [k "type",drawVar sz n,drawAlfCtx ia ctx,
		     eqD, drawExp ia e]
	  where b n ctx e = defB $ Type (n,(ctx,e))
	Axiom (n,(ctx,e)) ->
	    a3b b $
	    hboxcaD [k "postulate",drawVar sz n,drawAlfCtx ia ctx,
		     elemD, drawExp ia e]
	  where b n ctx e = defB $ Axiom (n,(ctx,e))
        Binding (n,e) ->
	    a2b (curry (defB . Binding)) $
	    hboxcaD [drawVar sz n,eqD,drawExp (iadata ia n emptyCtx e) e]
	Value (en,(ctx@(Ctx l ps),eval,etype)) ->
	    a5b bDef $
	    vboxlD' [sigD, hvIndentD 10 vindent defD]
	  where
	    (hctx,vctx) = apBoth (Ctx l) $ splitAt (sigHiding en) ps
	    sigD =hboxcaD' [drawVar sz en,drawAlfCtx ia vctx,elemD' sz,
	                    drawExp ia etype,blankD (Point 2 10),drawAlfCtx ia hctx]
	    --defD = hboxcaD [{-lhsD,-}eqD,drawExp ia' eval]
	    --lhsD = drawPat ia (app (EVar en) [EVar n|(n,_)<-ctx])
	    defD = drawEqn ia' (c en,(map fst ps,eval))
		   where
		     c (U.Var s) = U.Con s
		     ia' = iadata ia en ctx eval

            bDef n (Ctx l vctx) etype (Ctx _ hctx) (Branch (_,(_,eval))) = defB $ Value (n,(Ctx l (hctx++vctx),eval,etype))
	    --bDef n (Ctx _ ctx) etype eval = defB $ Value (n,(ctx,eval,etype))
            vindent = if isND then 30 else 0
	    --ia' = drfp ia -- to make the vertical ident bite, creates some unnecessary redrawing of non-ND expressions !!
	    --ia' = if isND then drfp ia else ia -- to make the vertical ident bite
	    isND = case eval of EProofOf _ _ -> True; _ -> False
      where
        defB = id

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
	      _             -> (vboxlD,wtD)
	  where
	    vboxl1D = vertlD . vis1D
	    hboxc1D = horizcD . vis1D
	    wtD (nameD:ctxD:theRestD) =
		wideOrTallD [hboxcaD [nameD,ctxD],hboxcaD theRestD]
    --}

    drawOpenArgs ia oas =
      asb OpenArgsS $
      commaSepD (map (drawOpenArg ia) oas)

    drawOpenArg ia (OpenArg ps n optt optas) =
        a3 ia b $
        hboxcaD [drawProps ps,optasD, drawVar{-NoSel-} (font_size ia)n,opttD]
      where
        --b optas n optt = (n,(optt,optas))::OpenArg
	b optas n optt = OpenArg ps n optt optas
	opttD = case optt of
		  Nothing -> a0b (Nothing::(Maybe Exp)) $ blankD 1 -- hmm
		  Just t -> a1b (Just::(Exp->Maybe Exp)) $
			    hboxcaD [elemD,drawExp ia t]
	optasD = case optas of
		  Nothing -> a0b (Nothing::(Maybe Var)) $ blankD 1 -- hmm
		  Just n2 -> a1b (Just::(Var->Maybe Var)) $
			     hboxcaD [drawVar sz n2,eqD]

    drawAlfCtx = drawAlfCtx' layoutStyle
    drawAlfCtxOpt = drawAlfCtx' . fromMaybe layoutStyle
    drawAlfCtx' Wide = drawCtx
    drawAlfCtx' Tall = drawCtxTall

    drawCtxTall ia (Ctx l ps@(_:_:_)) =
      a1 ia (Ctx l) $ emparen'' ia $ drawTable (elemD' sz) (drawExp ia) ps
    drawCtxTall ia ctx = drawCtx ia ctx

    drawCtx ia ctx@(Ctx l []) = a0top ctx $ blankD 5
--  drawCtx ia ctx = agroup "Bindings" . paren . hboxcaD . drawCtx' $ ctx
    drawCtx ia (Ctx l ps) = as0 (Ctx l . pre) . emparen'' ia . drawCtx' ia $ ps
      where
        pre [ListS xs] = map out xs
	pre xs = map out xs

    drawCtx' ia ctx =
	if compactOn
	then asb (concat::([Bindings]->Bindings)) $
	     commaSepD $ map drawBindings $ collectBySnd ctx
	else commaSepD (map (drawBinding ia) ctx)
      where
        drawBindings (xs,t) =
	    a2b b $
	    hboxcaD [asb VarsS $ hCommaSepD (map (drawVar sz) xs),
	             elemD,setDrfp (drawExp ia t)]
	  where b xs t = [(x,t)|x<-xs]::Bindings
     --}
        {- old:
	drawCtx' [] = []
	drawCtx' [b] = [drawBinding ia b]
	drawCtx' (b1:bbs@((_,t'):_)) =
	    hboxcaD' 0 [drawBinding' b1, commaD] : drawCtx' bbs
	  where
	    drawBinding' b@(x,t) =
	      if compactOn && t==t'
	      then a2b ((,)::(Var->Exp->(Var,Exp))) $
		   vis1D [drawVar sz x,drawExp (drfp ia) t]
	      else drawBinding ia b
        -}


    drawGoal (Goal g e)
        = hboxcaD [drawMeta ia g,   elemD,drawExp ia e]
    drawSolution (Solution g e)
	= hboxcaD [drawMeta ia g,   eqD,  drawExp ia e]
    drawConstraint (Constraint e1 e2)
	= hboxcD  [drawExp ia e1,   eqD,  drawExp ia e2]
    drawAssumption (Assumption var e)
        = hboxcaD (drawVar sz var:elemD:drawExp ia e':hctxD)
      where
        sz = font_size ia
	(args,res) = flatPi e
	(hargs,vargs) = splitAt (sigHiding var) args
	e' = piExp vargs res
	hctxD = if null hargs
		then []
		else [blankD (Point 2 10),emparen'' ia $ drawCtx' ia hargs]
    --}
    drawArgs :: IA -> Assoc -> Precedence -> (Exp->AlfDrawing,Exp->AlfDrawing)
    drawArgs ia assoc prec' = (drawExp' ctxl,drawExp' ctxr)
      where
	ctxl = ctx LHS ia
	ctxr = ctx RHS ia
	ctx = prec . InfixArg assoc prec'

--    drawExps = drawExps' drawExp
    drawAExps = drawExps' . drawAExp
    drawExps' draw = asb ExpsS . hboxcaD . map draw

    drawAExp = drawExp' . prec Atomic
    drawBExp = drawExp' . precBExp
    drawExp  = drawExp' . prec TopLevel
    drawPat ia = drawExp (ia{isPattern=True})
    prec p ia = ia {syn_ctx=p}
    precBExp = prec (InfixArg NonAssoc maxBound LHS)

    drawExpNoSel = drawExpNoSel' . prec TopLevel
    drawExpNoSel' ia = noSel . setDrfp . drawExp' ia
    --drawExp' = {-propIA-} drawExp''
    drawExp' :: IA -> Exp -> AlfDrawing
    drawExp' ia@(IA {font_size=sz}) e =
      -- "'" (haskell mode font-lock bug workaround)
      --trace (unwords ["drawExp'",show ia,show e]) $
      -- The ia arguments to the a1..a5 function below are meaningless!
      case e of
	EApp e1 e2 -> drawApp e1 e2
        EVar i -> a1 ia EVar $ idParen i $ drawEVar i
	EChar c -> a0 ia e $ k (show c)
	EString s -> a0 ia e $ k (show s)
	EInt n -> a0 ia e $ k (show n)
	ERat r -> a0 ia e $ k (show r)
	ECon c -> a0 ia e $ noParen $ drawConName sz c
	--ECon c es -> drawConApp ia Nothing c es
--	ECon c es -> a2 ia eCon $ paren' ia $ hboxcaD [drawConName sz c,drawAExps ia es]
--	ELCon c es -> a2 ia ELCon $ paren' ia $ hboxcaD [drawConName sz c,drawAExps ia es]
	EAbs nt e2 -> drawAbsExp ia e
	ETyped e t -> drawTypedExp e t --  && hideTrivialLet
	EProofOf t e -> drawProof t e  -- && proofStyle/=UglyProof
	ELet ds body -> drawLetExp ia ds body
	ECase e bs -> drawCase wideOrTallBraceD ia e bs

	EPi _ _ -> drawPiExp e
        ESort sort -> a1 ia ESort $ noParen $ drawSort sort
	ESum s -> drawSum wideOrTallBraceD ia s

	EProj e n -> a2 ia EProj $ noParen $ hboxcaD' 1 [drawAExp ia e,k ".",drawELabel n]
	EOpen e1 oas e2 ->
 	  a3 ia EOpen $
	  paren'' ia $
	  vboxlD [hboxcaD [k "open",drawExp ia e1,k "use",drawOpenArgs ia oas],
		  hboxcaD [k "in",drawExp ia e2]]
	ESig sig -> drawSigExp wideOrTallBraceD ia sig
	EStr str -> drawStruct wideOrTallBraceD ia str
	--ECom ix -> drawTheory wideOrTallBraceD ia ix
	EMeta meta -> annotM ia (const (syn e)) $ noParen $ drawMeta ia meta
	EAnnot a@(LayoutDir d) e' ->
	    a1 ia (EAnnot a) $
	    noSel $
	    setDrfp $
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
	      Just dr -> a2 ia b $ vis1D [drawText ia (dr (syn e')),drawAExp ia e']
	      Nothing -> a1 ia (EAnnot a) $ drawExpNoSel' ia e
	  where
	    b :: Exps -> Exp -> Exp
	    b txt e = EAnnot a e
	EAnnot a@(AltViewsBy (N fns)) e' ->
	    as ia b $
	    vboxlD (drawExp' ia e':
		    [drawText ia (dr (syn e'))
		     | Just dr<-map (flip lookup altdispfs) fns])
	  where
	    b :: Exps -> Exp
	    b (e:_) = EAnnot a e
	EAnnot a@(EComment s) e ->
	   a1 ia (EAnnot a) $ vboxlD' 1 [cmnt s,drawExp' ia e]
	EAnnot a@Hidden e' ->
	   a1 ia (EAnnot a) $ vis1D [d "...",drawAExp ia e']
	EAnnot a@(InEnv bs) e' ->
	    a2 ia b $ hboxcaD' 1 [drawExp' ia e',drawEnv (smaller ia) bs]
	  where
	    b e bs = EAnnot (InEnv bs) e
	    drawEnv ia =
	        embrace'' ia . drawTable (eqD' sz) (drawExp ia)
	      where sz = font_size ia
	EAnnot a e | isfold a->
	    a1 ia (EAnnot a) $ paren' ia $ hboxcaD' 1 [symb a,drawBExp ia e]
	  where
	    symb a = s' sz (symb' a)
	    symb' (FoldTo _)   = "¨"
	    symb' (UnfoldTo _) = "·"
	    symb' _ = " " -- shouldn't happen
	    isfold (FoldTo _) = True
	    isfold (UnfoldTo _) = True
	    isfold _ = False

	EAnnot a e -> a1 ia (EAnnot a) $ drawExpNoSel' ia e
{-
	  where ia' =
		  case a of
		    LayoutDir d -> ia { layoutdir=Just d }
		    _ -> ia
-}
      where
        s = s' sz
        d = d' sz

	paren''l = case syn_ctx ia of
		     InfixArg _ _ LHS -> paren
		     _ -> paren' ia

	paren'' ia = case syn_ctx ia of
		    InfixArg _ _ _ -> emparen'' ia
		    _ -> paren' ia

        paren' ia = case syn_ctx ia of
	           Atomic -> emparen'' ia
		   _      -> noParen

        paren = emparen'' ia

        idParen v = 
	  if isDistfix (idFixity (lookupArgOpts drawOpts v))
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
		    hboxcaD [drL e1,drawConName sz c,drR e2]
		  where
		    (drL,drR) = drawArgs ia assoc prec'
	    _ -> as ia b $ paren' ia $
	        wideOrTallD' (maxWidth drawOpts) dir
		   (drawConName sz c:map (drawAExp ia) es)
	      where b (n:es) = eCon (out n) (map out es)
	-}

        drawCase :: (AlfDrawing->AlfDrawing->AlfDrawing) -> IA -> Exp -> Branches -> AlfDrawing
	drawCase drawD ia e bs =
	    a2 ia ECase $
	    drawD (hboxcaD [k "case",drawExp ia e,k "of"])
		  (setNotOkDr $ drawBranches ia bs)

	drawSum drawD ia s = a1 ia ESum $ drawD (k "data") (drawData ia' s)
	  where ia' = ia --{ layoutdir=Nothing }
	        --b (ConstructorsS cs) = ESum ( cs)

	drawSigExp drawD ia sig =
	    as ia ESig $ drawD (k "sig") (drawSig sig)
	  where
	    ia' = ia --{ layoutdir=Nothing }
	    drawSig = vboxlrD . map drawSigPart

	    drawSigPart sp =
	      case sp of
	        SigField lbl e ->
		  a2 ia SigField $
		  hboxcaD [drawLabel sz lbl,elemD,drawExp ia' e]
		SigDef defb ->
		  a1 ia SigDef $ drawDefB ia Nothing defb

	       --(drawTable' (drawLabel sz) elemD (drawExp ia') sig)
	drawStruct drawD ia str =
	    a1 ia EStr $ drawD (k "struct") $
	    --drawTable eqD (drawExp ia') str --old
	    drawLetDecls ia str
	  where ia' = ia --{ layoutdir=Nothing }

	wideOrTallBraceD = wideOrTallBraceD' layoutStyle

	wideOrTallBraceD' dir =
	    case dir of
	      Tall -> tallD
	      Wide -> wideD
	  where
	    wideD preD subD = paren' ia $ hboxcD [preD, refMiddleD $ embrace'' ia subD]
	    tallD preD subD = paren'' ia $ vboxlD [preD, indentD 15 $ subD]

	drawPiExp = drawPiExp' ia Nothing
	drawPiExp' ia dir e@(EPi (p1:-e2) e3) =
	  if p1==noname
	  then drawArrow ia e2 e3
	  else if compactOn && p1/=noname
	       then drawPiExpCompact dir ia (flatPi' e)
	       else drawPiExpExpanded ia p1 e2 e3

	drawPiExpCompact dir ia (actx,e) =
	    a2 ia b $ paren'' ia $
	    wideOrTallD' (maxWidth drawOpts) dir
		 [hboxcaD [paren $ drawCtx' ia ctx,rarrowD],
		  setDrfp $ drawExp' ia e]
	  where b ctx e = piExp' as ctx e
	        --ia' = drfp ia
		(as,ctx) = unzip actx

        drawPiExpExpanded :: IA -> Var -> Exp -> Exp -> AlfDrawing
        drawPiExpExpanded ia p1 e1 e =
            a3 ia ePi $ paren'' ia $
	    wideOrTall0D [
	      hboxcaD [paren (hboxcaD [drawVar sz p1,elemD,drawExp ia e1]),
		       rarrowD],
	      setDrfp $ drawExp' ia e]
	  where ePi n t e = EPi (n:-t) e

        drawArrow :: IA -> Exp -> Exp -> AlfDrawing
        drawArrow ia e1 e2 =
	    a2 ia eFun $
	    paren'' ia $
	    hboxcaD [drawBExp ia e1,rarrowD,drawExp ia e2]

        drawAbsExp ia e@(EAbs nt e2) =
	  case syn_ctx ia of
	    Subgoal -> drawHyp ia (flatAbs' e)
	    _ -> if compactOn
		 then drawAbsExpCompact ia (flatAbs' e) -- handle annotations!!!
		 else drawAbsExpPlain ia nt e2

        drawHyp ia (ants,e) =
	    a2 ia b $ placeD [hypsD,drawExp ia e]
          where
	    ia' = smaller ia
	    hypsD = asb TypingsS $
		    embrack'' ia' $ vboxlD' 0 $ map (drawTyping ia') nts
	    (as,nts) = unzip ants
	    b nts e = absExp' as nts e
	    placeD = case proofStyle drawOpts of
		       BrorProof -> vboxlD' 1
		       _ -> hboxcaD

        drawAbsExpCompact :: IA -> ([([ExpAnnot],Typing)],Exp) -> AlfDrawing
        drawAbsExpCompact ia (ants,e) =
	    a2 ia b $ paren'' ia $
	    hboxcaD [lambdaD,drawHiddenTypings ia nts,rarrowD,
	             setDrfp $ drawExp ia e]
	  where
	    (as,nts) = unzip ants
	    b nts e = absExp' as nts e -- :: AlfExp

        drawAbsExpPlain :: IA -> Typing -> Exp -> AlfDrawing
        drawAbsExpPlain ia nt e =
	  a2 ia EAbs $ paren'' ia $
	  wideOrTall0D [ hboxcaD [lambdaD,drawTyping ia nt,rarrowD],
			setDrfp $ drawExp ia e]

        drawEVar :: Var -> AlfDrawing
        drawEVar n@(U.Var s) = annotB (const (VarS n)) $ drawName' sz (DO.Var s)
        drawELabel n@(Label s) = annotB (const (LabelS n)) $ drawName' sz (DO.Name s)

	drawApp = drawApp' Nothing ia
	drawApp' dir ia e1 e2 = drawFlatApp' dir ia (flatApp e1 e2)
	drawFlatApp' :: Maybe LayoutDirection -> IA -> (Exp, [Exp]) -> AlfDrawing
	drawFlatApp' dir ia (f,args) =
	    plainApp ia $
	    case (fixity,map splitAnnots visargs) of
	      (Postfix prec',[arg]) ->
		(drawPostfix prec' fhid (aa arg),[])
		-- Do something more clever with infix ops & hidden arguments...
	      (Infix assoc prec',e1:e2:es) | hidingOn ->
		(drawInfix2 assoc prec' fhid (aa e1) (aa e2),map aa es)
	      (Infix assoc prec',[e1,e2]) ->
		(drawInfix2 assoc prec' fhid (aa e1) (aa e2),[])
	      (Infix assoc prec',[e1]) ->
		(drawInfix1 assoc prec' fhid (aa e1),[])
	      (Quantifier sd,[(a,EAbs n e20)]) ->
		(drawQuantifierAbs sd fhid a n e20,[])
	      (Quantifier sd,[e2]) ->
		(drawQuantifier sd fhid (aa e2),[])
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
		  EVar v -> varOptions v
		  ECon c -> conOptions c
		  _ -> (0,Nonfix)
	    sz = font_size ia
	    sz'   = sz-2
	    small = ia { font_size=sz', syn_ctx=TopLevel }
	    big   = ia { font_size=sz+3, syn_ctx=Atomic }
	    c = hCenterD -- spacer is lost when replacing a subexpr!!
	    bl = blankD 2

            plainApp :: IA -> (IA->AlfDrawing,Exps) -> AlfDrawing
	    plainApp ia (d,[]) = d ia
	    plainApp ia (d,args) =
		as ia b $ paren' ia $
		wideOrTallD' (maxWidth drawOpts) dir
			(d ia':map (drawAExp ia) args)
	      where b (e:es) = appc e es
		    ia' = precBExp ia

	    hiddenAExp fhid ia = hiddenApp fhid (prec Atomic ia)
	    hiddenBExp fhid ia = hiddenApp fhid (precBExp ia)

            hiddenApp :: (Exp,Exps) -> IA -> AlfDrawing
	    hiddenApp (f,[]) ia = drawExp' ia f
	    hiddenApp (f,args) ia =
		as ia b $ vis1D (drawExp' ia f:map (drawAExp ia) args)
	      where b (e:es) = app e es

            drawTuple :: (Exp,Exps) -> Exps -> IA -> AlfDrawing
	    drawTuple fhid es ia =
		as ia b $ emabrack'' ia $ hboxcaD $
		hiddenD (hiddenApp fhid ia):
		[commaSepD (map (drawExp ia) es)]
	      where b (e:es) = app e es

            drawFraction fhid e1 e2 ia =
	        a3 ia b $
		placedD ((moveRefsS (Point 0 5) `compS` refEdgesS) `spacerP`
			 verticalP' 1 `spacersP` [fpartS,refMiddleS,fpartS]) $
		boxD [hiddenD (hiddenApp fhid ia),
		      drawExp ia e1, hLineD, drawExp ia e2]
	      where
	        b f e1 e2 = f `EApp` e1 `EApp` e2

            drawProofGoal (f,hargs) e1 e2 ia = drawProof' (Just r) b e1 e2
	      where b e1 e2 = r `EApp` e1 `EApp` e2
		    r = app f hargs
		    -- Since r isn't drawn, it mustn't contain meta variables...

	    -- pre: viscnt == length visargs
	    drawDistfix assoc prec' fhid@(f,hidargs) visargs viscnt =
	        case (apFst groupArgs . nameOpts) `map` funname f of
		  Just (ss,bmsrc) | argcnt == viscnt ->
		      (\ia->as ia b $
		            parenDistfix ia assoc prec' ss $
		            hboxcaD' 1 $
		            --paragraphD $ -- doesn't look that nice...
                            hiddenD (hiddenApp fhid ia):
		            dist1D (mix ss (permute visargs p)),
		       [])
	            where
	              argcnt = mixfixArity p
		      p = permutation ss
		      invp = invperm p
		      b (e:es) = appc e (permute es invp)
		      (drL,drR) = drawArgs ia assoc prec'
		      dist1D (Right e:es) = drL e:distD es
		      dist1D es = distD es
                      distD [] = []
	              distD [Right e] = [drR e]
	              distD (Right e:es) = drawExp ia e:distD es
		      distD (Left s:es) = dw s:distD es
		      dw s = drawName''' d' s' sz (s,bmsrc)

		  _ -> (hiddenApp fhid,visargs)
	      where
		funname (EVar v) = Just (DO.name v)
		funname (ECon c) = Just (DO.name c)
		funname _ = Nothing

		parenDistfix ia assoc prec' ss =
		  if isDelimited ss
		  then noParen
		  else parenOp ia assoc prec' -- prec' ??

	    drawPostfix prec' e1 e2 ia =
		a2 ia (flip EApp) $
	        parenOp ia NonAssoc prec' $
		hboxcaD [drL e2,hiddenBExp e1 ia]
		-- Assoc vs NonAssoc?
	      where (drL,_) = drawArgs ia Assoc prec'

	    drawInfix1 assoc prec' e1 e2 ia =
		a2 ia (flip EApp) $ paren $ hboxcaD [drL e2,hiddenBExp e1 ia]
	      where (drL,_) = drawArgs ia assoc prec'

	    drawInfix2 assoc prec' op e1 e2 ia =
		a3 ia b $ parenOp ia assoc prec' $
		wideOrTallD' (maxWidth drawOpts) dir
		  [hboxcaD [drL e1,hiddenBExp op ia],drR e2]
	      where (drL,drR) = drawArgs ia assoc prec'
		    b e1 op e2 = app op [e1,e2]

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

	    drawQuantifierAbs sd q a nt e ia =
		a3 ia qApp $
	        qParen sd $
	        hboxcaD' 2 [hiddenAExp q ia,
			    drawTyping'' (not sd) ia nt,
			    qBodyD sd (drawExp ia e)]
	      where qApp q nt e = EApp q (aa (a,EAbs nt e))

	    drawQuantifier sd q e ia =
		a2 ia qApp $
	        qParen False $
	        hboxcaD' 2 [hiddenAExp q ia,
			    drawName' sz n,
			    qBodyD False (boxD [setDrfp $ drawBExp ia e,drawName' sz n])]
	      where qApp q e = EApp q e
	            n = DO.Var "x" -- !! hmm, should pick an unused variable
		    -- !! Confusing to show a variable that doesn't exist!

            qParen sd = if sd || quantifierDots
			then paren'' ia
			else paren' ia
            qBodyD sd d =
	      if sd || quantifierDots
	      then boxD [k' sz ".",d]
	      else paren d

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
        
	parenOp ia assoc prec =
	  if needParen (syn_ctx ia) assoc prec
	  then paren
	  else noParen

        drawLetExp ia ds body =
	  a2 ia ELet $ paren'' ia $
	  vboxlD [hboxcaD [k "let",drawLetDecls ia ds],
		  hboxcaD [k "in",drawExp ia body]]

        drawTypedExp = drawTypedExp' ETyped
        drawTypedExp' bld e t =
	  if hideTrivialLet drawOpts
	  then a2 ia bld $
	       vis1D [drawExpNoSel' ia e,drawExp ia t]
	  else case e of
	         EMeta _ ->
		      setNotOkDr $ 
		      a2 ia bld $
		      paren'' ia $
		      hboxcaD' 1 [setNotOkDr $ drawExp ia e,noSel $ drawExp ia' t]
		    where ia' = ia {font_size=font_size ia-2}
		 _ -> a2 ia (flip bld) $
		      paren'' ia $
		      vboxlD [embrack'' ia $ drawExp ia t,drawExp ia e]

        drawProof :: Exp -> Exp -> AlfDrawing
	drawProof = drawProof' Nothing EProofOf
        --drawProof' :: Maybe Exp->(Exp->Exp->Exp) -> Exp -> Exp -> AlfDrawing
        drawProof' optgoalfun bproof t e =
	  case proofStyle drawOpts of
	    NDProof   -> drawNDProof e' t
	    BrorProof -> drawBrorProof e' t
	    _ -> drawTypedExp' (flip bproof) e t
	  where
	    e' = splitAnnots e
	    ndgoal = ndgoalVar

	    ruleNameD rule hidden = setDrfp $ drawExp' ia' (app rule hidden)
	      where ia' = prec (NDGoal optgoalfun) (smaller ia)

            drawSubgoal = drawExp' (prec Subgoal ia)

	    drawNDProof (annots,e) t =
	      case flat e of
		(rule,subgoals) ->
		    a3 ia b $
		    -- Spacer for improved positioning in zoomed mode only:
		    spacedD (hvAlignS aCenter aBottom) $
		    noParen $
		    hboxcaD' 3 [ruleD, refMiddleD (ruleNameD rule hidden)]
		  where
		    b es t rule = eProofOf annots t (app rule es)
		    ruleD = 
		      refEdgesD $
		      placedD (verticalP' 1 `spacersP`
		                [noRefsS `compS` hCenterS,
				 refMiddleS,
				 fpartS])
		      $ boxD [subgoalsD, hLineD, drawExp ia t]

		    subgoalsD =
			asb ExpsS $ hboxdD (map drawSubgoal visible)
		      where
			hboxdD [] = blankD 5
			hboxdD ds =
			  placedD (spacersP (horizontalP' 15)
				  (repeat (hvAlignS aCenter aBottom))) $
			  boxD ds
		    (hidden,visible) = splitAt hidecnt subgoals
		      where
			hidecnt =
			  case rule of
			    EVar v {-| hidingOn-} ->
				hideCnt (lookupArgOpts drawOpts v)
			    _ -> 0

	    drawBrorProof (annots,e) t =
		case flat e of
		  (rule,subgoals) ->
		      a3 ia b $ noParen $
		      vboxlD' 0 [drawExp ia t,
			      tableD' 0 2 [
			        if null visible then upArrowTailD else upArrowD,
				--s "­", -- up arrow in the symbol font
				spacedD (hvMarginS bsep bsep) $
				ruleNameD rule hidden,
				subgoalsD]]
		    where
		      bsep = Point 0 7
		      b t rule es = eProofOf annots t (app rule es)
		      subgoalsD = asb ExpsS $
		                  --hCenterD $ vboxlD' 15 $
				  boxD $
		                  zipWith drawSubgoal' [1..] visible
		      drawSubgoal' i subg =
		          boxD [connectD,
				hboxD [hrLineD,drawSubgoal subg]]
		        where connectD = if i<viscnt
					 then forkRightD
			        	 else lowerRightD
		      viscnt = length visible
		      (hidden,visible) = splitAt hidecnt (subgoals)
		        where
			  hidecnt =
			    case rule of
			      EVar v {-| hidingOn-} ->
				  hideCnt (lookupArgOpts drawOpts v)
			      _ -> 0

	    eProofOf annots t e = bproof t (attachAnnots annots e)

	flat (EApp e1 e2) = flatApp e1 e2
	flat e = (e,[])
   
    drawMeta ia meta =
	--v' sz $ g' ("?"++show (meta::Int))
	if meta<0
    	then q
 	else hboxcaD' 0 [q, v' (sz-2) $ g' $ show (meta::Int)]
      where
        q = v' sz $ g' "?"
	g' = fgD [metacolor,"black"] . g
	sz = font_size ia

    drawData ia = drawConstrs
      where
        drawConstrs :: Constructors -> AlfDrawing
	drawConstrs =
	  asb ConstructorsS . vboxlD . map drawConstr
	drawConstr :: Constructor -> AlfDrawing
	drawConstr (Constr (n,(ctx,restrs))) =
	    a3 ia b $ hboxcaD [c $ drawConNameSel sz n,
			       drawAlfCtx ia ctx,
			       drawRestrs restrs]
	  where b n ctx rs = Constr (n,(ctx,rs))
	        c = spacedD vCenterS

        drawRestrs [] = a0b ([]::Exps) (blankD 1)
	drawRestrs es =
	    case data_lhs ia of
	      Just lhs -> a1b b $ hboxcaD [elemD,drawExpNoSel ia (app lhs es)]
	        where
		  b = reverse . take (length es) . reverse . snd . flatApp'
	      _ -> asb ExpsS $ hboxcaD (elemD:k "_":map (drawAExp ia) es)

	{-
        drawConArgs :: AlfConstructorArgs -> AlfDrawing
	drawConArgs (Ctx _ [],[]) = a0b ((emptyCtx,[])::AlfConstructorArgs) (blankD 1)
	drawConArgs (args,cmnt) =
	    a2b b $ hboxcaD [drawAlfCtx ia args,drawComment cmnt]
	  where b ctx cmnt = (ctx,cmnt)::AlfConstructorArgs
	drawComment cmnt@[] = a0top (cmnt::EqRestr) $ blankD 1
	drawComment cmnt = --a1 ia cmnt $
			   --agroup "Equality Constraints" $
	                   embrack (drawTable' (drawExp ia) eqD (drawExp ia) cmnt)
	-}

{-
    outData :: Syntax -> Table (Context ExpAnnot,AlfComment)
    outData (ListS cs) = map outA cs
      where outA (PairS n a) = (out n,outBinds a)
	    outA x = error ("outA "++show x)
	    outBinds (BindsS bs) = (bs,[])
	    outBinds x = error ("outBinds "++show x)
    outData x = error ("outData "++show x)
-}

    drawBranches :: IA -> Branches -> AlfDrawing
    drawBranches ia = as0 BranchesS . vboxlD . map (drawBranch ia)

    drawBranch :: IA -> Branch -> AlfDrawing
    drawBranch = drawBranch' drawPat rarrowD
    drawEqn ia = drawBranch' drawPat' eqD ia . Branch
      where drawPat' ia e =
              case flatApp' e of
	        (ECon (U.Con n), es) -> drawPat ia (app (EVar (U.Var n)) es)

    drawBranch' :: (IA->Exp->AlfDrawing)->AlfDrawing->IA->Branch->AlfDrawing
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
	    setDr = setNotOkDr . setDrfp
	    drawMatch (lhs@(con,vars),match) =
	      case match of
	        Simple (pat,e) ->
		    setDr $
		    a2 ia b $
		    --hboxcaD [patD ia pat,sepD,drawExp ia' e]
		    wideOrTallD [hboxcaD [patD ia pat,sepD],
				 setDrfp $ drawExp ia e]
		  where b = bBranch lhs
		Nested f x ms -> setDr $ a1 ia b $ asb BranchesS $ msD
		  where
		    b bs = Branch (con,(vars,f $ ECase (EVar x) bs))
		    msD = if null ms
		          then blankD 5
			  else vboxlD (map drawMatch ms)
        --}
        bBranch :: (Con,[Var]) -> Exp -> Exp -> Branch
	bBranch (con,vars) pat e = Branch (con,(vars,e)) -- note: pat unused

        drawBranchPlain :: Branch -> AlfDrawing
	drawBranchPlain (Branch (con{-@(U.Con s)-},(vars,e))) =
	    a2 ia b $
	    hboxcaD [patD ia (eCon con (map EVar vars)),sepD,drawExp ia e]
	  where b = bBranch (con,vars)
{-
          case (idFixity (lookupArgOpts drawOpts (DO.Con s)),vars) of
	   (Infix _ _,[v1,v2]) -> 
	        an0 id branch $
	        hboxcaD [drawName sz v1,drawConName sz con,drawName sz v2,
		        rarrowD,drawExp ia e]
	      where
		branch [v1,c,v2,e]= syn (Branch (out c,([out v1,out v2],out e)))
	   _ -> an0 id branch $
	        hboxcaD [drawConName sz con,drawVars sz vars,
			rarrowD,drawExp ia e]
	     where
	       branch [c,vs,e] = syn (Branch (out c,(out vs,out e)))
-}
    --}
    wideOrTallD = wideOrTallD' (maxWidth drawOpts) Nothing
    wideOrTall0D = wideOrTallD'' 0 (maxWidth drawOpts) Nothing

    drawBinding :: IA -> Binding -> AlfDrawing
    drawBinding ia = drawBinding' (drawVar sz) (elemD' sz) (drawExp ia)


    drawTable = drawTable' (drawVar sz)
    drawTable' :: (a->AlfDrawing)->AlfDrawing->(b->AlfDrawing)->[(a,b)]->AlfDrawing
    drawTable' lhs sep rhs =
      agroup "Bindings" . vboxlrD . map (drawBinding' lhs sep rhs)
    drawBinding' :: (a->AlfDrawing)->AlfDrawing->(b->AlfDrawing)->(a,b)->AlfDrawing
    drawBinding' lhs sep rhs (n,e) =
      apair "Binding" $ hboxcaD [lhs n,sep,rhs e]

{-
    drawTable = drawTable' (drawVar sz)
    drawTable' lhs sep rhs =
      agroup "Bindings" . tableD 3 . map (drawBinding' lhs sep rhs)
    drawBinding' lhs sep rhs (n,e) =
      apair "Binding" $ boxD [lhs n,sep,rhs e]
--}
    drawConName sz n = noSel $ drawConNameSel sz n

    drawConNameSel sz n@(U.Con s) =
        --setIA (ia{isCon=True}) $
	annotSa (const (ConS n)) $
        drawName'' c' s' sz (DO.Con s)

    drawTyping' = drawTyping'' compactOn
    drawTyping'' b = if b then drawHiddenTyping else drawTyping

    drawTyping :: IA -> Typing -> AlfDrawing
    drawTyping ia (n:-t) =
        a2 ia (:-) $ hboxcaD [drawVar sz n,elemD' sz,drawExp ia t]
      where
        sz = font_size ia

    drawHiddenTypings ia = asb TypingsS . hboxcaD . map (drawHiddenTyping ia)
    drawHiddenTyping ia (n:-t) =
        a2 ia (:-) $ vis1D [drawVarNoSel ia n,setDrfp $ drawExp ia t]

    drawVarNoSel ia n = noSel (drawVar sz n)
      where
        sz = font_size ia

    drawVars sz = asb VarsS . hboxcaD . map (drawVar sz)
    drawVar sz n@(U.Var s) = annotSa (const (VarS n)) $ drawName' sz (DO.Var s)
    drawCon sz n@(U.Con s) = annotSa (const (ConS n)) $ drawName'' c' s' sz (DO.Con s)
    drawLabel sz n@(Label s) = annotSa (const (LabelS n)) $ drawName' sz (DO.Name s)

    drawName' = drawName'' v' s'
    drawName'' n s sz =  drawName''' n s sz . nameOpts
    drawName''' normal symbol sz (dstr,bmsrc) = i dstr
      where
	i = case bmsrc of
	      UseNormalFont -> normal sz . g
	      UseSymbolFont -> symbol sz
	      UseImageFile -> g . BitmapFile

    nameOpts name =
	(fromMaybe (nameStr name) (displayAs opts),bitmapSource opts)
      where opts = lookupArgOpts drawOpts name
	
    --drawSort n@(Sort s) = annotSa (const (SortS n)) $ v s
    drawSort n@(Sort s) = a0b (SortS n) $ v s

    drawText ia = asb ExpsS . cmntD . drawParas
      where
        drawParas = vboxlD' 0 . map drawPara

        --drawPara (n,ws) =
	--   indentD (defaultSep*n) $ paraD $ map drawTextItem $ ws

        drawPara (PlainPara ws) = paraD $ map drawTextItem ws
	drawPara (NestedPara txt) = indentD (3*defaultSep) $ drawParas txt

        paraD = placedD (paragraphP' (Point defaultSep 0)) . boxD

	drawTextItem ti =
	  --trace (show ti) $
	  case ti of
	    TSyntax stx -> setNotOkDr $ draw drawOpts ia altdispfs stx
	    TPlain s -> g s
	    TSymbol Nothing symb -> s' (font_size ia) symb
	    TSymbol (Just font) symb -> fontD font (g symb)
	    TVar s -> v s

    sigHiding var =
      if hidingInSignatures && hidingOn
      then fst (varOptions var)
      else 0

    varOptions (U.Var s) = idOptions (DO.Var s)
    conOptions (U.Con s) = idOptions (DO.Con s)

    idOptions name =
      case lookupArgOpts drawOpts name of
        ArgOptions {hideCnt=hideCnt, idFixity=idFixity} -> (hideCnt,idFixity)


    -- Eta expansions because of the monomorphism restriction:
    a0 x = an b0 x
    a1 x = an b1 x
    a2 x = an b2 x
    a3 x = an b3 x
    a4 x = an b4 x
    a5 x = an b5 x
    an bn ia = annotS ia.bn

    a0b x = anb b0 x
    a1b x = anb b1 x
    a2b x = anb b2 x
    a3b x = anb b3 x
    a4b x = anb b4 x
    a5b x = anb b5 x
    anb bn = annotB.bn
    asb x = anb syns x

    a0top x = a0 iaTop' x
    an0 bn = an bn iaTop'

    agroup d = annotB ListS
    apair d = annotB (\[x,y]->PairS x y)
    as0 x = an0 syns x
    as x = an syns x

    --a1' = an b1'
    --a2' = an b2'
    --b1n f = syn.f.out.head

    annotB b = annotD (alfAnnot' False False iaTop') b  -- dummy annot
    annotS ia b = annotD (alfAnnot' True False ia) b
    annotStop = annotS iaTop'
    annotSa x = annotS (prec Atomic ia) x
    annotM ia b = annotD (alfAnnot' True True ia) b

    --setIA ia = changeAnnot $ \ a -> a{inherited=ia}
    {- old:
    setIA ia (LabelD (Annot alfAnnot build) d) =
      LabelD (Annot (alfAnnot{inherited=ia}) build) d
    setIA ia d = trace "setIA failed" d
    -}

    setDrfp = changeAnnot $ \ a -> a{redrawFromParent'=True}
    setNotOkDr = changeAnnot $ \ a -> a{okRedrawPoint'=False}

    noSel = setIsSelectable False

    setIsSelectable b = changeAnnot $ \ a -> a{is_selectable=b || is_hole a}
    {- old:
    setIsSelectable b (LabelD (Annot alfAnnot build) d) =
      LabelD (Annot (alfAnnot{is_selectable=b || is_hole alfAnnot}) build) d
    setIsSelectable _ d = trace "setIsSelectable failed" d
    -}

    iaTop' = iaTop drawOpts

    smaller ia = ia { font_size=font_size ia - 1}

    -- propagate only some attributes
    --propIA dr ia = setIA ia . dr (nodrfp $ okdr ia)

    --whboxD = west.hboxD
    --whboxw = whboxD.map west
    --whboxw' sep = whboxD' sep.map west
    --whboxD' sep =west.hboxD' sep

    --errorD = k "¿¿"

    hCommaSepD = hboxcaD' 0 . intersperse (d", ")
    commaSepD = paragraphD . map (hboxcaD' 0) . group2 . intersperse (d", ")
    group2 = chopList (splitAt 2)
    paragraphD = placedD (paragraphP' (Point 0 defaultSep)) . boxD
    commaD = d ","
    rarrowD = ds "®"
    lambdaD = ds "l"
    elemD = elemD' sz
    eqD = eqD' sz
    elemD' s = if uglyelem
	       then d' s "::"
	       else ds' s "Î"
    --eqD' s = d' s "="
    eqD' s = ds' s "º"

    --wk = west.k

    v,cmnt :: String -> AlfDrawing
    v = hardAttribD (font VarFont).g
    k',c',s',d',ds' :: Graphic x => Int -> x -> AlfDrawing
    k' s = hardAttribD (font' s KeyWordFont).g
    c' s = hardAttribD (font' s ConFont).g
    v' :: Int -> AlfDrawing -> AlfDrawing
    v' s = hardAttribD (font' s VarFont)
    s' s = hardAttribD (font' s SymbolFont).g
    d' s = hardAttribD (font' s DelimFont).g
    --ds' s = d' s
    ds' s = hardAttribD (font' s DelimSymbolFont).g
    cmnt = cmntD.g
    cmntD = hardAttribD (font CommentFont)
    font = font' sz
    font' s f = fontsOpt drawOpts f s

    k = k' sz
    --s = s' sz
    ds = ds' sz
    d = d' sz
    --ds = ds' sz
    sz = font_size ia
    --l = fontG litFont.g

    embrack'' = embrack' . d' . font_size
    embrace'' = embrace' . d' . font_size
    emabrack'' = emabrack' . d' . font_size
    emparen'' = emparen' . d' . font_size

changeAnnot f (LabelD (Annot alfAnnot build) d) =
    LabelD (Annot (f alfAnnot) build) d
changeAnnot f d = trace "changeAnnot failed" d

metacolor = argKey "metacolor" "blue3"
uglyelem = argFlag "uglymembershipsymbol" False
