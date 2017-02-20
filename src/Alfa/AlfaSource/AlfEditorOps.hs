module AlfEditorOps(
    AlfState,AlfEditMsg(..),AlfEditFunc(..),TextInputParser(..),Plug.CompleteFunc,
    drawAlfSyntax,drawAlfTopSyntax,AlfAnnot,
    newFile,empty,startEditor,
    deleteSomething,giveSomething,refineSomething,pasteSomething,
    clickRefineByCopying,refineByClicking,loadParsedModule,
    appendDecl,changeLayout,changeState,describe,
    PE.Error
  ) where

--- Proof engine stuff
--import BetaReduce(betaReduce)
import qualified ProofEngine as PE
import ProofmonadOps
import FileConv(expP,ctxP,varP,conP,labelP,manyP,someP,mapP,commaSeqP,declsP,prSyntax,prSyntax1line,prCon)
import UAbstract
import UAnnots
import AlfAbstract
import AbstractOps
import AlfModules
import USubstitute(avoidNameCapture)

--- Alfa stuff
import AlfOps
import AlfSyntax
import SubstMeta(recordSubstMeta)
--import MapAnnot(mapEAnnot)
import Annot()
import EditFunc(editLocalAbs,editGlobal,editError,editShowMessage,editShowMenu,EditMenu(..))
import EditMonad
import AlfEditMonad
import EditAnnot
import Fonts(AlfFonts(..),AlfFontName(..),fontsize)
--import Gedit(build)
import DrawAlf
import qualified DrawOptions as DO
import Fixity(SyntacticContext(..),Fixity(..))
import AlfState
import Variables --(update,compVar)
import qualified AlfaPlugin as Plug
import Plugins

--- Natural Language stuff
--import ParseForAlfa(alfaparser)
--import AlfaToEnglish(alfaToEnglish) -- buggy?

--- Util stuff
import ListUtil(assoc,chopList,mapFst,mapSnd)
import List(nub,partition)
import HO(apFst,apSnd)
import Maybe(mapMaybe,isJust)
import Utils2(mix)
--import Monad(ap)

--- Fudgets stuff
import Fud

--- debugging:
import Debug2(trace)
--import Show(show_exp)

default(Int)
#ifdef __HASKELL98__
#define map fmap
#endif

parseExpStrG = parseStrEd expP
parseCtxStrG = parseStrEd ctxP

empty = ModuleS (Module [])

startEditor alfstate syntax =
  editLocalAbs $ \ _ ->
  do storeEd alfstate
     return syntax

newFile filename =
  editLocalAbs $ \ _ ->  -- editLocal? Only makes sense on the top level...
  do updateEd (setCurrentFile filename . resetState)
     return empty

emptyFile =
  editLocalAbs $ \ _ ->  -- editLocal? Only makes sense on the top level...
  do updateEd resetState
     return empty

give goal =
  editWithParsedText ("?"++show goal++" = ") $
  checkedInput expP $
  parse_give

parse_give str = editMetaGiveG $ parseExpStrG str

{-
give_NL goal =
    editWithSlowParsedText ("Define ?"++show goal++" in English:") $
    englishInput $ editGoalG . tryGiveEnglishG
  where
    englishInput ed s =
      case alfaparser s of
	[] -> Left (-1,"Natural language parser failed")
	es -> Right (ed (map (mapEAnnot NaturalLang) es))
    tryGiveEnglishG es k = tryG (PE.give k) es `handleG` errG
       where
         errG msg = trace (show es) $
	            errorEd (msg++": "++
			    mix (map (prSyntax1line (const Nothing)) es) ", ")
-}

giveUnchecked goal =
  editWithParsedText ("?"++show goal++" = ") $
  checkedInput expP $ \ str ->
  editLocalAbs $ \ _ ->
  map ExpS (edPE $ parseExpStrG str)

editExpAsText s =
  editWithParsedTextDef "Edit expression:" s $
  checkedInput expP $
  pasteStringOverExp

editCtxAsText s =
  editWithParsedTextDef "Edit context:" s $
  checkedInput ctxP $
  pasteStringOverContext

editDeclAsText = editDeclAsText' "Edit definition:"
editDeclAsText' prompt s =
  editWithParsedTextDef prompt s $
  checkedInput declsP $
  pasteDecl . Left

editRenameVar n@(Var s) =
  editWithParsedStringDef ("Rename "++s++" to:") (prSyntax n) $
  parsedInput varP $ editReplace

editRenameTyping (n@(Var s):-t) =
  editWithParsedStringDef ("Rename "++s++" to:") (prSyntax n) $
  parsedInput varP $ editReplace . (:-t)

editRenameCon n@(Con s) =
  editWithParsedStringDef ("Rename constructor "++s++" to:") (prCon n) $
  parsedInput conP $ editReplace

refineByClicking dOpts True = clickRefineWithCaseOrOpen
refineByClicking dOpts False = clickRefineByCopying dOpts

clickRefineByCopying dOpts (state,optAnnot,selectionS) clickedS =
    case clickedS of
      ImportS (Import filename) -> browse filename
      _ -> case selectionS of
             ExpS (EMeta g) ->
	       case coerceToExp clickedS of
	         ExpS e ->
	           case stripAnnots e of
	             EVar var -> refineWith var
		     ESort sort -> editMetaGive e
	             ELet  _ _ -> refineWithLet
	             ECase _ _ -> refineWithCase
		     ESig  _   -> refineWithSig
	             ESum  _   -> refineWithData
		     EPi (x:- _) _ -> namedFuncType' [x]
		     EAbs (x:- _) _ -> namedAbs' [x]
		     ECon con -> editRefineCon g con
                     EProj _ _ -> editRefineExp g e
		     --ELCon name _ -> refineWith name
	             _ -> editError msg3
		 _ -> editError msg2
	       where refineWith = editRefineVar g
             _ -> editError msgSelectPlaceholder
  where
    msg2 = "No middle mouse button operation defined for "++typeof clickedS
    msg3 = "Don't know what to do with the expression you clicked on."

    editRefineCon g con = editRefineExp g (eCon0 con)
    editRefineExp g e = editRefineTst (tstPlainRefine dOpts g e)

    editRefineVar g name = editRefineTst (tstRefine state optndrule g name)
      where
        optndrule = maybe Nothing (proofGoalRule . syn_ctx . inherited) optAnnot

    editRefineTst tst = elimEd state (edPE tst) fst editError


msgSelectPlaceholder =
  "Select a place holder first to refine with the middle mouse button."

clickRefineWithCaseOrOpen (state,optAnnot,selectionS) clickedS =
    case selectionS of
      ExpS (EMeta _) ->
	case coerceToVar clickedS of
	  VarS n -> editMeta . refineCaseOrOpenG . EVar $ n
	  _ -> editError msg2
      _ -> editError msgSelectPlaceholder
  where
    msg2 = "Select a placeholder and shift-click with the middle button on an identifier to create a case experssion"


refineWithRefine dOpts =
  editWithParsedText "Refine - enter the expression to refine with" $
  parsedInput expP (editRefine' dOpts)

pasteError syntax ctx =
  unwords ["Trying to paste", typeof syntax, "where", ctx, " is required."]

pasteErrorG syntax ctx = errorEd (pasteError syntax ctx)
editPasteError syntax ctx = editError (pasteError syntax ctx)

pasteExp (Left s) = parse_give s
pasteExp (Right syntax) =
  case coerceToExp syntax of
    ExpS e -> editMetaGive e 
    _      -> editPasteError syntax "an expression"

pasteUncheckedExp (Left s) =
  editLocalAbs $ \ _ ->
  map ExpS (edPE $ parseExpStrG s)
pasteUncheckedExp (Right syntax) =
  case coerceToExp syntax of
    e@(ExpS _) -> editLocalAbs $ \ _ -> return e
    _          -> editPasteError syntax "an expression"

coerceToVar syntax =
  case syntax of
    ExpS e ->
      case stripAnnots e of
       EVar n -> VarS n
       _ -> syntax
    TypingS (n :- _) -> VarS n
    _ -> syntax

coerceToExp (ConS n) = ExpS (eCon0 n)
--coerceToExp (NameS n@(Name ('#':_))) = ExpS (ELCon n [])
coerceToExp (VarS n) = ExpS (EVar n)
coerceToExp (LabelS (Label s)) = ExpS (EVar (Var s))
coerceToExp (TypingS (n :- _)) = ExpS (EVar n)
coerceToExp (SortS sort) = ExpS (ESort sort)
coerceToExp (DeclS (Decl _ [DefA _ (Open (e,oargs))])) =
	ExpS (EOpen e oargs m0)
coerceToExp (DeclsS ds) = ExpS (ELet ds m0)
coerceToExp s = s

pasteOverExp = either pasteStringOverExp pasteSyntaxOverExp

pasteStringOverExp s = editReplaceExpG (parseExpStrG s)

pasteSyntaxOverExp syntax =
  case coerceToExp syntax of
    ExpS e -> editReplace e
    _      -> editPasteError syntax "an expression"

pasteOverContext = either pasteStringOverContext pasteSyntaxOverContext

pasteStringOverContext s =
  editGlobal $ \ path drawing oldsyntax ->
  do ctx <- edPE (parseCtxStrG s)
     replaceSyntax ctx path drawing oldsyntax

pasteSyntaxOverContext syntax =
  case {-coerce...-} syntax of
    s@(ContextS ctx) -> editReplace s
    _ -> editPasteError syntax "a context"

pasteDecl pasted =
    editGlobal $ \ path drawing _ ->
    do cdecls <- edPE (getDecl pasted)
       state <- loadEd
       let part' = boxD (map (drawAlfTopSyntax state) cdecls)
       reload (replacePart drawing path part')
  where
    getDecl pasted =
      case pasted of
	Left str -> parseStrEd declsP str
	Right syntax ->
	  case syntax of
	    DeclS  decl  -> return [decl]
	    DeclsS decls -> return decls
	    ModuleS (Module decls) ->
	      return (filter (not.isImportDecl) decls)
	    ExpS e ->
	      case stripAnnots e of
	        EOpen e oas _ -> return [decl' [defA (Open (e,oas))]]
	        ELet ds _ -> return ds
		_ -> err
	    _ -> err
	  where err = pasteErrorG syntax "declaration(s)"

deleteSomething syntax =
  case syntax of
    ModuleS _ -> emptyFile         -- editReplace empty
    DeclsS  _ -> editReplace (DeclsS [])
    DeclS   _ -> editGlobal delElem
    ConstructorS _ -> editGlobal delElem
    BranchS _ -> editGlobal delElem
    OpenArgS _ -> editGlobal delElem
    ExpS e ->
      case e of
        EMeta _ -> editError "Deleting a place holder doesn't change anything"
	_ -> editDeleteExp
    _ -> editError ("Can't delete "++typeof syntax)

giveSomething syntax =
  case syntax of
    ExpS (EMeta g) -> give g
    _ -> editError "Select a place holder before using the command Give"

refineSomething dOpts syntax =
  case syntax of
    ExpS (EMeta _) -> refineWithRefine dOpts
    _ -> editError "Select a place holder before using the command Refine"

pasteSomething syntax =
  case syntax of
    ExpS (EMeta _) -> editByPasting pasteExp
    ExpS _         -> editByPasting pasteOverExp
    HoleS          -> editByPasting pasteDecl
    DeclS _        -> editByPasting pasteDecl
    ContextS _     -> editByPasting pasteOverContext
    _              -> editError "Sorry, can't paste here"

parsedLabels0 = parsedInput (manyP labelP)
parsedNames = parsedInput (someP varP)

editNames prompt cont =
  editWithParsedStringEx prompt "Example: x y z" $
  parsedNames cont

editLHSs = editLHSs' "Enter LHSs for new declaration"
editLHSs' prompt cont =
  editWithParsedStringEx prompt "Example: even n, odd n" $
  parsedLHSs cont

namedAbs =
  editNames "Names for abstraction" $ namedAbs'

namedAbs' names = editMetaGive $ uAbsExp names m0

fullAbs = editGoalG abstractAllG

namedFuncType = editNames "Names for abstraction" namedFuncType'

namedFuncType' names =
    editMetaGive $ piExp (zip names m0s) m0

refineWithLet =
    editLHSs' "Let - enter LHSs for local declaration" $ \ lhss ->
    editMetaGive $ buildLet (buildDecl' lhss)
  where
    buildLet decl = ELet [decl] m0

refineWithNDStyleLet = editRefine (ndGoalRefinement Nothing)

ndGoalRefinement Nothing = R $ EProofOf m0 m0
ndGoalRefinement (Just rule) = R $ app rule [m0,m0]

refineWithTrivialLet = editRefine $ R $ ETyped m0 m0

refineWithSig =
    editWithParsedStringEx "Sig - enter names of signature parts"
                         "Example: fst snd" $
    parsedLabels0 $ \ labels ->
    editMetaGive $ ESig (zipWith SigField labels m0s)

extendSig oldsig =
    editWithParsedStringEx "Extend sig - enter names of new signature parts"
                         "Example: fst snd" $
    parsedLabels0 $ \ labels ->
    editReplaceExpG $ return $ ESig (oldsig ++ zipWith SigField labels m0s)

refineWithData = refineWithIData 0

refineWithIData n =
  editWithParsedStringEx "Data - enter contructors with their argument names"
		       "Example: Nil, Cons x xs" $
  parsedData $ \ constrs ->
  editMetaGive $ buildData n [] constrs

refineWithLit ex b =
  editWithParsedStringEx "Enter literal"
                         (show ex) $
  readInput $ editMetaGive . b

refineWithCharLit = refineWithLit 'a' EChar
refineWithStringLit = refineWithLit "Hello, world!" EString
refineWithIntLit = refineWithLit 42 EInt

editReplaceLit b old =
  editWithParsedStringDef "Edit literal:" (show old) $
  readInput $ editReplace . b

editReplaceCharLit = editReplaceLit EChar
editReplaceStringLit = editReplaceLit EString
editReplaceIntLit = editReplaceLit EInt

extendData oldsum =
  editWithParsedStringEx
    "Extend data - enter new contructors with their argument names"
    "Example: Nil, Cons x xs" $
  parsedData $ \ constrs ->
  editReplaceExpG (return (buildData 0 oldsum constrs))
        -- Number of indexes? Should consult the type of the idata expression!!

parsedData cont = parsedLHSs (cont . mapFst conname)
  where conname (Var s) = Con s

parsedLHSs = parsedInput (map f `mapP` commaSeqP (someP varP))
  where f (n:ns) = (n,ns)


#ifdef IAGDA
buildData n [] [] | n>0 = EEmptyIndSum
#endif
buildData n0 oldData constrs =
    ESum (oldData++buildConstrs constrs)
  where
    buildConstrs = map Constr . mapSnd (\args->(args,ms)) . buildLHSs
    ms = replicate n m0
    n = case oldData of
	  Constr (n,(_,es)):_ -> length es
	  _ -> n0

buildLHSs = map buildLHS
  where
    buildLHS (c,args) = (c,ctx (zip args m0s))

extendCase e oldbranches =
    editWithParsedStringEx
      "Extend case - enter new contructors with their argument names"
      "Example: Nil, Cons x xs" $
    parsedData $ editReplaceExpG . return . buildCase 
  where
    buildCase constrs = ECase e (oldbranches++newbranches)
      where
        newbranches = map b constrs
	b (c,args) = Branch (c,(args,m0))

refineWithCase =
  editWithParsedString "Case - enter the expression to analyse" $
  checkedInput expP $ \ str ->
  editMeta $ \ gphs ->
  parseExpStrG str >>= \ e ->
  refineCaseG e gphs


refineWithOpen =
  editWithParsedString "Open - enter the expression to open" $
  checkedInput expP $ \ str ->
  editMeta $ \ gphs ->
  parseExpStrG str >>= \ e ->
  refineOpenG e gphs

appendPackage = appendDecl' buildPackage
appendPostulate = appendDecl' buildPostulate
appendDecl = appendDecl' buildDecl'

appendDecl' build =
    editLHSs newDecl
  where
    newDecl lhss =
      editGlobal $ \ _ _ old@(ModuleS (Module olddecls)) ->
      do let decl0 = build lhss
         --(~[decl],ms) <- stepsEd [decl0]
         decls <- appendDeclsEd [decl0]
         return [([],ModuleS (Module (olddecls++decls)))]


appendOpenDecl =
    editWithParsedString "Open - enter the expression to open" $
    checkedInput expP $ \ str ->
    editGlobal $ \ _ _ old@(ModuleS (Module olddecls)) ->
    do e <- parseExpStrG str
       opendecl <- edPE (PE.openTLhack e)
       decls <- appendDeclsEd [opendecl]
       return [([],ModuleS (Module (olddecls++decls)))]

checkTermination peState ds complete =
  editShowMessage $
  case PE.query peState (PE.checkTermination ds) of
    Left err -> Left err
    Right () -> Right ("Termination Check succeeded"++
		       if complete
		       then "."
		       else if length ds==1
		            then ", but the definition is incomplete."
		            else ", but the definitions are incomplete.")

changeLayout = changeState . setAlfHiding

changeState stMod =
  editGlobal $ \ _ _ decls ->
  do updateEd stMod
     return [([],decls)]

describe state optAnnot (syntax:ancestors) phs =
    apSnd EditMenu $
    addConstraintStuff $
    addPluginStuff $
    case syntax of
      ExpS e -> describeExp e
      DeclS  (Comment _) -> (dG "Comment",mapFst Right (commentMenu syntax))
      DeclS  d@(Decl c _) -> (dG descr,mapFst Right (declMenu syntax++checkMenu))
	where
          descr = "Declaration defining "++
	          unwords (map DO.nameStr (namesDecl d)) ++
		  declinfo d ++
		  if c then "" else ", not type checked"
	  checkMenu = if c
		      then checkTermMenu [d]
		      else [(checkAgain,cmd "Check Again")]
	  checkAgain = editGlobal $ \ _ drawing _ -> reload drawing
      --CDeclsS _ -> (dG $ "Top Level Declarations"++ phsinfo,
      --	    mapFst Right (declsMenu syntax))
      --DeclS  d -> (dG $ "Declaration"++declinfo d,mapFst Right (declMenu syntax))
      DeclsS ds -> (dG $ "Declarations"++phsinfo,
		    mapFst Right (declsMenu syntax++checkTermMenu ds))
      ModuleS (Module []) -> (dG "Empty Module",mapFst Right appendDeclMenu)
      ModuleS (Module ds) -> (dG $ "Module"++phsinfo,
				mapFst Right (moduleMenu++checkTermMenu ds))
      ImportS (Import filename) -> (dG $ "Import "++filename,
				    mapFst Right importMenu)
	where
	  importMenu = [(browse filename,sh ("Browse "++filename))]
      ContextS _ ->
        (dG "Context", mapFst Right
	  [(editCtxAsText (prSyntax syntax), cmd "Edit as text")
	   ])
      ConstructorS c -> (dG "Constructor",mapFst Right delDeclMenu) -- !!
      BranchS c      -> (dG "Branch",     mapFst Right delDeclMenu) -- !!
      OpenArgS oas   -> (dG "Open argument",
			 mapFst Right (delDeclMenu++ -- !!
			               propsMenu' id oas++
				       typeSignMenu id oas))
      VarS n@(Var s) ->
          (dG ("Variable, name: "++s),
	   [editLayoutCmd n,
	    (Right $ deleteLayoutOpts n,sh ("Default layout options for "++s++"...")),
	    (Right $ editRenameVar n,sh ("Rename "++s++"..."))])
      ConS n@(Con s) ->
          (dG ("Constructor, name: "++s),
	   [editLayoutCmd n,
	    --(Right $ deleteLayoutOpts n,sh ("Default layout options for "++s++"...")),
	    (Right $ editRenameCon n,sh ("Rename "++s++"..."))])
{-
      LabelS n@(Label s) ->
          (dG ("Label, name: "++s),
	   [editLayoutCmd n
	    --(Right $ deleteLayoutOpts n,sh ("Default layout options for "++s++"...")),
	    --(Right $ editRenameCon n,sh ("Rename "++s++"..."))
	    ])
-}
      TypingS xt@(n@(Var s):-t) ->
        (g $ drawAssumption (n,t),
	 [editLayoutCmd n,
	  (Right $ deleteLayoutOpts n,sh ("Default layout options for "++s++"...")),
	  (Right $ editRenameTyping xt,sh ("Rename "++s++"..."))])

      HoleS -> (dG "Between Declarations",mapFst Right holeMenu)
        where
	  holeMenu =
	      [addDeclItem,
	       insertPackage,
	       insertPostulate,
	       (insertOpenDecl,cmd "Insert a new open declaration"),
	       insertComment,
	       (editByPasting pasteDecl,cmd "Paste a declaration")]

          insertComment =
	      (editDeclAsText' "Insert a comment" "--",
               cmd "Insert a new comment")

          insertPostulate =
	    addDeclItem'' "Insert a new postulate" buildPostulate

          insertPackage = addDeclItem'' "Insert a new package" buildPackage

          insertOpenDecl =
	    editWithParsedString "Open - enter the expression to open" $
	    checkedInput expP $ \ str ->
	    editGlobal $ \ path drawing oldsyntax ->
	    do e <- parseExpStrG str
	       opendecl <- edPE (PE.openTLhack e)
	       replaceSyntax (DeclS opendecl) path drawing oldsyntax

      _ -> (dG $ typeof syntax,[])
  where
    peState = csps state
    dOpts = drawOptions state
    draw s = drawAlfTopSyntax state s
    drawConstraint (e1,e2) = draw (Constraint e1 e2)
    drawSolution (v,e) = draw (Solution v e)
    drawGoal (v,e) = draw (Goal v e)
    checked' = maybe True (checked . inherited) optAnnot
    ispattern = maybe False (isPattern . inherited) optAnnot
    isNDgoal = isJust optndrule --maybe False (isNDGoal . syn_ctx . inherited) optAnnot
    optndrule = maybe Nothing (proofGoalRule . syn_ctx . inherited) optAnnot
    --unfoldgoals = if unfoldGoals dOpts then 5 else 0

    {-
    --drawAssumption :: (Var,Exp) -> Drawing AlfAnnot Gfx
    drawAssumption =
      case get menuLangVar state of
        BuiltinLang -> builtin
	PluginLang pl -> draw . EAnnot (asTextBy pl) . applyit
	  where
	    applyit (v,t) = app (EVar v) . map typedmeta . fst . flatFun $ t
	    typedmeta (EPi xt t) | DO.autoAbstract dOpts = EAbs xt (typedmeta t)
	    typedmeta t = ETyped m0 t
      where
        builtin (v,t) = draw (Assumption v t)
    -}

    drawAssumption (v,t) = drawMenuItem (syn (Assumption v t))

    drawMenuItem stx =
      case get menuLangVar state of
        BuiltinLang -> draw stx
	PluginLang pl ->
	    case stx of
	      ExpS e -> drawExp e
	      AssumptionS (Assumption v t) -> drawExp (applyit v t)
	      _ -> draw stx
	  where
	    drawExp = draw . EAnnot (asTextBy pl)

	    applyit v t = app (EVar v) . map typedmeta . fst . flatFun $ t
	    typedmeta (EPi xt t) | DO.autoAbstract dOpts = EAbs xt (typedmeta t)
	    typedmeta t = ETyped m0 t

    editLayoutCmd n =
      (Right $ editLayoutOpts n,
       sh ("Change layout options for "++DO.nameStr n++"..."))

    addConstraintStuff (d,menu) =
        (vboxlD [d,constraintsD], constraintSolveMenu ++ menu)
      where
        constraints = nub $ PE.inspect peState PE.constraints
	cnt = length constraints
	constraintsD = vboxlD $ map drawConstraint constraints

        constraintSolveMenu =
	  [(Right $ editMetas (PE.give g e), shSol sol)
	   | sol@(g,e) <- PE.inspect peState PE.suggestSolution
	   ]

	shSol sol =
	    if quickmenuFlag
	    then sh (strSol sol)
	    else (strSol sol,G $ hboxcaD [g "Solve",drawSolution sol])

        strSol (g,e) = "= Solve ?"++show g++" = "++prSyntax1line e

{- old:
        constraintSolveMenu =
	  [(Right $ editMetas (PE.solveConstraint t n),
	    sh ("= Solve constraint "++show n++" with method "++show t))|
	   n<-[0..cnt-1], t<-[0,2,4,1,3,5],
	   isRight $ PE.query peState (PE.solveConstraint t n)]
-}
    addPluginStuff (d,menu) =
	(vboxlD (d:mapMaybe (map g) plugds),concat plugMenus ++ menu)
      where
	(plugds,plugMenus) = unzip . map plugDescr . get pluginVar $ state
	plugDescr (name,Plugin st m) =
	    --trace (show (Plug.save m st)) $
	    apSnd (map convMenu) (Plug.describe m st (metaEnv state) (syntax:ancestors))
	  where
	    convMenu (s,cmd) = (convCheckCmd cmd,sh' cmd s)
	    --sh' (Plug.Give _) s = sh s -- unnecessary strictness...
	    sh' _  s = cmd s
	    convCmd cmd =
	      case cmd of
	        Plug.Give e -> editMetaGive e
		Plug.TryGive e cont ->
		  convCmd $ cont $ either Just (const Nothing) $
		                          convCheckCmd (Plug.Give e)
		Plug.Replace syntax' ->
		  case (syntax,syntax') of
		    (DeclS _,DeclsS _) -> pasteDecl (Right syntax')
					  -- This gives you a recheck...
		    _ -> editLocalAbs (const  $ return syntax')
		Plug.ReplaceRecheck syntax' ->
		  case (syntax,syntax') of
		    (DeclS _,DeclsS _) -> pasteDecl (Right syntax')
		    _ -> editReplace syntax'
		Plug.ReplaceState st' -> updPlug (name,Plugin st' m)
		Plug.EditWith (Plug.EArgs multiline prompt optdef parse complete) cont ->
	           --editWithParsedStringDef prompt def editfunc
		     editWithText multiline prompt
				  (optdef,"",editfunc,True)
				  complete
		   where
		     editfunc s =
		       case parse s of
		         Right st' -> Right $ convCmd (cont st')
			 Left (pos,err) -> Left (("user input",pos),err)
	        Plug.Menu m ->
		  editShowMenu $ EditMenu $
		    [(convCheckCmd c,(str,G (drawMenuItem stx)))|(c,(str,stx))<-m]
		Plug.Message msg -> editShowMessage msg
		Plug.GetPEState cont -> convCmd (cont peState)
		Plug.SaveFileAs optpath contents -> saveFileAs optpath contents
		Plug.DisplayGfx gfx -> displayGfx gfx
		Plug.GetDrawOptions cont -> convCmd (cont (altdispfs state) (drawOptions state))
            updPlug = changeState . update pluginVar . replace 

            convCheckCmd cmd =
	      case cmd of
{-
               -- This causes unnecessary strictness,
               -- just to show a red light when a plugin is buggy...
	        Plug.Give e ->
		  case syntax of
		    ExpS (EMeta g) ->
	              elimEd peState (PE.give g e)
			     (const (Right (convCmd cmd)))
			     Left
		    _ -> Left "Plugin bug: command only applies to meta variables"
-}
		_ -> Right (convCmd cmd) -- No feedback for other commands...

    declinfo (Decl _ defs) = 
      phsinfo ++
      case nub [details | DefA (_,_,details) _ <- defs] of
        [optDetail] -> ", showing "++detailinfo
	  where detailinfo =
		  case optDetail of
		    Nothing -> "the default declaration view"
		    Just JustNames -> "defined names only"
		    Just NamesAndTypes -> "type signature"
		    Just CompleteDecls -> "complete definition"
		    Just (DeclAsTextBy (pn,fn)) -> unwords ["by",pn,fn]
		    Just (DeclAltViewsBy _) -> "multiple views"
	_ -> ""
    phsinfo =
        if null phs
      	then ""
      	else " with "++
	     flip mix ", " 
	      ((if null invis
	      	then []
	      	else [show icnt ++ " hidden hole"++is])++
	       (if tcnt==icnt
	      	then []
	      	else [show tcnt ++ " hole"++ts++" total"]))
      where invis = filter (not.fst) phs
	    icnt = length invis
	    is = s icnt
	    tcnt = length phs
	    ts = s tcnt
	    s n = if n==1 then "" else "s"

    describeExp (EMeta g) = describeMeta g
    describeExp e = (padD 2 $ hboxcaD expDescr,menu)
      where
	menu = if ispattern then [] else expMenu

        expDescr =
	  case e' of
	    ETyped e t ->
	        descrT t:dG descrL:descr1 (as1++as2)++[dG (descr2 e')]
	      where (as2,e') = splitAnnots e
	    _ -> dG descrU:descr1 as1++[dG (descr2 e')]
	  where (as1,e') = splitAnnots e

        descrT = drawAlfTopSyntax state

	descr2 e =
	  (++phsinfo) $
	  case e of
	    ESort (Sort s) -> ", the sort "++s
	    EPi _ _ -> ", function type "
	    ESum _ -> ", data type"
	    ESig _ -> ", record type"
	    EVar (Var s) -> ", a variable named "++s
	    ECon (Con s) -> ", a constructor named "++s
	    EApp e1 e2 ->
	      case flatApp e1 e2 of
	        (EVar (Var s),_) -> ", application of "++s
		(ECon (Con s),args) ->
		  ", application of constructor "++s
		_ -> ""
	    EAbs _ _ -> ", abstraction"
	    EProj _ _ -> ", projection"
	    EStr _ -> ", record"
	    --ETyped _ _ -> " with explicit type signature" -- handled above
	    -- Literals:
	    EChar _ -> ", characer literal"
	    EString _ -> ", string literal"
	    EInt _ -> ", int literal"
	    _ -> ""

        descr1 = concatMap descr1'
	descr1' a =
	  case a of
	    LayoutDir Wide -> [dG ", wide"]
	    LayoutDir Tall -> [dG ", tall"]
	    Hidden -> [dG ", hidden"]
	    Was (N m) -> [dG ", was",gt]
	      where
	        gt = either e (curry drawGoal m . snd) $
		     PE.query peState (PE.goal 0 m)
		e x = trace x $ dG "??"
	    _ -> []

	(descrU,descrL) =
	  if ispattern
	  then ("Pattern","pattern")
	  else ("Expression","expression")

	expMenu =
            mapFst Right (
	    (editDeleteExp,cmd "Delete"):
	    (editExpAsText (prSyntax e), cmd "Edit as text"):
	    createLet e:
	    createLetDef e:
	    trivialLetCmds e ++
	    layoutCmds e) ++
	    computeCmd e ++
            mapFst Right (
	    case stripAnnots e of
	      ELet com exp -> [addLetDecl com exp]
	      ESum constrs -> [(extendData constrs,cmd "Add constructors")]
	      ESig sig ->  [(extendSig sig,cmd "Add fields to signature")]
	      ECase e bs -> [(extendCase e bs,cmd "Add branch")]
	      EChar c -> [(editReplaceCharLit c,litcmd "Edit character literal")]
	      EString s -> [(editReplaceStringLit s,litcmd "Edit string literal")]
	      EInt n -> [(editReplaceIntLit n,litcmd "Edit Int literal")]
	      EOpen eo oa eb ->
	        case stripAnnots eb of
		  EVar v@(Var s) | v `elem` map opened oa ->
		    [(editReplace (EProj eo (Label s)),
		      cmd "Simplify to a projection")]
		    where opened (OpenArg _ lbl _ (Just alias)) = alias
			  opened (OpenArg _ lbl _ _) = lbl
		  _ -> []
	      _ -> [])
	  where
	    layoutCmds e =
	      case e of
		EAnnot (LayoutDir Wide) e' -> [mkv e',mkdefault e',hide e]
		EAnnot (LayoutDir Tall) e' -> [mkh e',mkdefault e',hide e]
		EAnnot Hidden e -> [reveal e]
		EAnnot (AsTextBy _) e' -> [mkdefault e']
		EAnnot (AltViewsBy _) e' -> [mkdefault e']
		_ -> [mkh e,mkv e,hide e]
	    mkh = mk "wide" . el Wide
	    mkv = mk "tall" . el Tall
	    mkdefault = mk "default"
	    el = EAnnot . LayoutDir
	    reveal e = (replaceExpL e,cmd "Reveal hidden expression")
	    hide e = (replaceExpL (EAnnot Hidden e),cmd "Hide expression")
	    mk s e = (replaceExpL e,sh ("Show "++s++" expression view"))
	    replaceExpL = replaceExpLocal

            trivialLetCmds e =
	        case stripAnnots e of
		  EApp _ _ -> both
		  EVar _ -> both
		  EProofOf _ e -> delND e
		  ETyped e _ -> delTa e
		  --EMeta _ -> tycmd -- metavars are handled in describeMeta...
		  _ -> []
	      where
	        both = ndcmd ++ tycmd
	        ndcmd = [(editReplace (EProofOf m0 e), cmd "ND Style Proof")]
		tycmd = [(editReplace (ETyped e m0),
		          cmd "Type annotation")]
		delND = del "Plain proof term"
		delTa = del "Remove type annotation"
		del txt e =[(editReplace e,cmd txt)]

            computeCmd e =
	      case apFst (partition isWas) (splitAnnots e) of
	        ((as1@(Was (N m):_),as2),e) ->
		   [(editComputeExp m (as1++as2) e,cmd "Compute")]
		_ -> []

            editComputeExp m as e =
	      case PE.query peState (PE.compute (Just m) e) of
		Right e' -> Right (editChangeExp ae')
		  where ae' = attachAnnots as e'
		Left err -> Left err

	    isWas (Was _) = True
	    isWas _ = False

	addLetDecl ds = addDeclItem' . bldLet ds
	createLet = addDeclItem'' "let expression with this body" . bldLet []
	bldLet decls e = ExpS . flip ELet e . (decls++) . (:[]) . buildDecl'

	createLetDef e = (editReplace e',cmd "Wrap")
	  where
            e' = ELet [decl' [defA d]] m0
	    d = Value (x,(emptyCtx,e,m0))
	    (x,_) = avoidNameCapture itVar m0 e

    describeMeta g =
          if checked'
	  then elimEd state (edPE $ getGoal g) descrGoal (\err-> (dG err,[]))
	  else (dG "Place holder in unchecked expression",uncheckedMenu)
	where
	  uncheckedMenu =
	    (Right $ giveUnchecked g,cmd "Give without type checking"):
	    (Right $ editByPasting pasteUncheckedExp,
	     cmd "Paste an expression without type checking"):
	    []

          getGoal g =
	    do let i = if DO.unfoldGoals dOpts then 1 else 0
	       (ctx,t) <- PE.goal i g
	       --t <- if i>0 then PE.compute (Just g) t else return t -- test!!
	       intros <- if isNDgoal then return [] else PE.intro g
	       return (t,intros,filter ((/=ndgoalVar).fst) ctx)

	  descrGoal ((t@e,intros,scope),_) =
	      (padD 2 $ drawGoal (g,e),
	       cmds' ++ map (refineAssMenu g) scope
	      )
	     where
	       cmds' = if DO.onlyRefine dOpts then [] else cmds
	       cmds =
		 --(Right $ give g,cmd "Give"):
		 --(Right $ give_NL g,cmd "Give in English"):
		 --(Right refineWithRefine,cmd "Refine"):
		 --(Right $ editByPasting pasteExp,cmd "Paste an expression"):
		 (Right refineWithLet,cmd "let"):
		 (Right $ refineWithTrivialLet,cmd "Type annotation"):
		 (Right refineWithCase,cmd "case"):
		 (Right refineWithOpen,cmd "open"):
		 (projectionsCmd,cmd ". Projections..."):
		 (if isNDgoal then []
		  else [(Right $ refineWithNDStyleLet,cmd "ND Style Proof")]
		       {-++ mapFst Right conintro-}) ++
		 mapFst Right refineAlts ++
		 map intro intros ++
		 mapFst Right setintro

               intro e = (tstGive e,
			  (prSyntax1line e,G $ drawAlfTopSyntax state  e))
	         where
	           tstGive e =
		     PE.query peState (PE.give g e >> return (editMetaGive e))

	       refineAlts =
		   (if abstraction `elem` altNames
		   then [(namedAbs,cmd "Abstraction with given names"),
		         (fullAbs,cmd "Abstract many")]
		   else []) ++
		   (if cmdprefix++"Function Type" `elem` altNames
		    then [(namedFuncType,cmd "Function Type with given names"),
		          (indepFuncType,cmd "Independent Function Type")]
		    else [])
		   ++refineAlts''
		 where
		   altNames = map (fst.snd) refineAlts'
		   refineAlts'' = filter ((/=abstraction).fst.snd) refineAlts'
		   refineAlts' = refineExpMenu g (map fst scope) -- used names
		   abstraction = cmdprefix++"Abstraction"
		   indepFuncType = editRefine $ R $ eFun m0 m0

	       setintro =
                   sig ++ take 1 (data' ++ idata) ++ char ++ string ++ int
	         where
		   sig = setintro' (ESig []) (refineWithSig,cmd "sig")
		   data' = setintro' (ESum []) (refineWithData,cmd "data")
		   char = setintro' (EChar 'a') (refineWithCharLit,litcmd "Character literal")
		   string = setintro' (EString "a") (refineWithStringLit,litcmd "String literal")
		   int = setintro' (EInt 42) (refineWithIntLit,litcmd "Int literal")
		   setintro' exp entry = tryRefine (PE.give g exp) [entry]

#ifdef IAGDA
                   idata = setintro' EEmptyIndSum (refineWithIData n,cmd "data")
		     where
		       n = goalArity g
		       goalArity g =
		         case PE.query peState (abstractAllG g) of
			   Left _ -> 0 -- hmm?
			   Right (args,_) -> length args
			         
#else
                   idata = []
#endif
	       {-
	       --intro = tryRefine (introG g) [(editIntro,cmd "Intro")]
	       --  where editIntro = editMeta refineIntroG
	       ---}

               tryRefine rG menu = elimEd peState rG
				          (const menu)
					  (const [])

               projectionsCmd =
	           if null projs
		   then Left "No simple projections are possible"
		   else Right (editShowMenu $ EditMenu projs)
		 where
		   projs =
		     [(PE.query peState $ tstPlainRefine dOpts g e,
		       (prSyntax1line e,G $ draw e))|
		       (x,t)<-scope,
		       lbl <- sigLbls t,
		       let e = EProj (EVar x) lbl
		     ]
		   sigLbls t =
		     elimEd peState (PE.compute (Just g) t)
		                    (sigLbls' . fst)
			            (const [])
		   sigLbls' (ESig sig) = [lbl|SigField lbl _<-sig]
		   sigLbls' _ = []

    addDeclItem = addDeclItem' buildDecl'
    addDeclItem' build =  addDeclItem'' "Insert a new declaration" build
    addDeclItem'' txt build = (addDecl1,cmd txt)
      where
	addDecl1 = editLHSs (editGlobal . addDecl2)
	addDecl2 lhss path drawing olddecls =
	     reload (replacePart drawing path part')
	   where
	     stx = build lhss
	     part' = drawAlfTopSyntax state (syn stx)

    refineAssMenu g (name,t) =
        (optEd,shAss (name,t))
      where
        optEd = PE.query peState (tstRefine state optndrule g name)

        strAss (Var s,t) = s -- ++ " : " ++ prSyntax1line t
	shAss a@(name,t) =
	    if quickmenuFlag
	    then sh (strAss a)
	    else (strAss a,G $ drawAssumption a)

    refineExpMenu g usedNames =
        mapMaybe tryRef (expRefinements usedNames)
      where
        tryRef (desc,r) =
	  elimEd peState (buildRefinementG r >>= PE.give g)
	         (const (Just (editRefine r,cmd desc)))
	         (const Nothing)

    moduleMenu = appendDeclMenu++moveArgsMenu
    commentMenu d = delDeclMenu ++ declAsTextMenu d
    declAsTextMenu d =[(editDeclAsText (prSyntax d),cmd "Edit as text")]
    checkTermMenu ds = [(checkTermination peState ds (null phs),cmd "Check Termination")]

    declMenu d = delDeclMenu++declAsTextMenu d++declsMenu d
    declsMenu d = moveArgsMenu++typeSignMenu mapDecls d ++ propsMenu d ++ detailsMenu

    delDeclMenu = [(editGlobal delElem,cmd "Delete")]
    appendDeclMenu = [(appendDecl,cmd "Append a new declaration"),
		      (appendPackage,cmd "Append a new package"),
		      (appendPostulate,cmd "Append a new postulate"),
		      (appendOpenDecl,cmd "Append a new open declaraton")]
    moveArgsMenu = [(editArgsToLhs,cmd "Move parameters to LHS"),
		    (editArgsToRhs,cmd "Move parameters to RHS")]
    typeSignMenu m d = [(editAddTypeSign m d,cmd "Add type signatures"),
		       (editRmTypeSign m d, cmd "Remove type signatures")]
    detailsMenu = [(editDetailsJ JustNames,cmd "Show defined names only"),
		   (editDetailsJ NamesAndTypes,cmd "Show type signatures only"),
		   (editDetailsJ CompleteDecls,cmd "Show complete definitions"),
		   (editDetailsN, cmd "Show default declaration view")]
  
    propsMenu = propsMenu' mapDecls
    propsMenu' mapP d =
	(ch (const []),cmd "Remove all explicit properties"):
	[(ch (addProp p),sh ("Make "++show p)) | p<-allProps]
      where
        ch f = editReplace (mapP (changeProps f) d)
	addProp p ps = p:filter (notExcludedBy p) ps

        -- Hmm. Is it at all meaningful to have more than one property?!
	notExcludedBy Abstract = (`notElem` [Abstract,Concrete])
	notExcludedBy Concrete = (`notElem` [Concrete,Abstract])
	notExcludedBy Private = (`notElem` [Private,Public])
	notExcludedBy Public = (`notElem` [Public,Private])

    editArgsToRhs = editLocalAbs $ returnEd . mapDecls declArgsToRhs
    editArgsToLhs = editLocalAbs $ returnEd . mapDecls declArgsToLhs
    editDetails d = editLocalAbs $ returnEd . mapDecls (changeDeclDetail d)
    editDetailsJ = editDetails . Just
    editDetailsN = editDetails Nothing

    editAddTypeSign m d = editReplace (m addTypeSign d)
    editRmTypeSign  m d = editReplace (m rmTypeSign d)

    editDecls f = editReplace . mapDecls f

    editLayoutOpts x =
        editLayout (n,lookupAlfArgOpts state n)
      where n = DO.name x

    deleteLayoutOpts = changeState . changeOptions . delete
      where
        delete n = update DO.idsOptionsOpt $ filter ((/=DO.name n).fst)

    sh x = (x,G x)
    cmd x = (cmdprefix++x,G x)
    litcmd x = ('\'':x,G x)
    dG = hardAttribD (DO.fontsOpt dOpts LabelFont fontsize).bugfix.padD 2.g
      where bugfix = id --vboxlD.(:[]) -- workaround problem with top level spacers!!

tstRefine state optndrule g name =
    PE.setTerminationCounter tsttermcnt >>
    case optndrule of
      Just ndrule ->tstSpecial (editRefineNDGoal ndrule name hide)
      _ -> case DO.idFixity argOpts of
	     Quantifier _ -> tstAutoRefine
	     _ -> if DO.autoAbstract dOpts
		  then tstAutoRefine
		  else tstPlainRefine dOpts g (EVar name)
  where
    hide = DO.hideCnt argOpts
    argOpts = lookupAlfArgOpts state name
    dOpts = drawOptions state

    tstAutoRefine = tstSpecial (editRefineAutoAbstract name hide)
    tstSpecial ed = refineExactG g (EVar name) >> return ed

tstPlainRefine dOpts g e = refineG dOpts g e >> return (editRefine' dOpts e)

mapDecls f syntax =
    case syntax of
      DeclS  decl  -> DeclS (f decl)
      DeclsS decls -> DeclsS (map f decls)
      ModuleS m -> ModuleS (mapModuleDecls f m)
      ExpS (ELet ds e) -> ExpS (ELet (map f ds) e)
      ExpS (EStr ds) -> ExpS (EStr (map f ds))
      _ -> syntax -- return an error indication?


editDeleteExp = editReplaceExpG (return m0)
--editReplaceExp = editReplace . ExpS
replaceExp = replaceSyntax . ExpS
replaceExpLocal e = editLocalAbs $ \ _ -> return (ExpS e)

-- For semantics preserving rewrites:
editChangeExp = editLocalAbs . const . return . ExpS

editReplaceExpG g =
  editGlobal $ \ path drawing olddecls ->
  do e <- edPE g
     replaceExp e path drawing olddecls

expRefinements usedNames =
    [sort "Set",
     sort "Type",
     --sort "Theory",
     --("theory",R 0 (\ _ ->ECom Unit)),
     ("Abstraction",R (EAbs (n:-m0) m0)),
     ("Function Type",R (EPi (n:-m0) m0))]
  where n:_ = [ n | n<-names, n `notElem` usedNames]
        names = [Var [n] | n<-['a'..'z']]
	sort s = (s,R (ESort (Sort s)))

refineCaseG = refineGoalG . flip tacCaseG
refineOpenG = refineGoalG . flip tacOpenG
refineCaseOrOpenG = refineGoalG . flip tacCaseOrOpenG
--refineIntroG = refineGoalG introG

editSimpleRefine' = editGoalG . flip refineExactG
editRefine' dOpts = editGoalG . flip (refineG dOpts)
editSmartRefine' = editGoalG . flip refineSmartG

editRefineNDGoal ndrule = editRefineAutoAbstract' postG
  where
    postG g =
      --trace ("editRefineNDGoal "++show g) $
      buildRefinementG (ndGoalRefinement ndrule) >>= \ e ->
      PE.give g e

editRefineAutoAbstract = editRefineAutoAbstract' (const (return ()))

editRefineAutoAbstract' postG name hideCnt =
    editGoalG $ \ g ->
    --trace ("editRefineAutoAbstract "++show g) $
    giveApp g name >>= \ args ->
    -- Find visible subgoals that weren't instantiated automatically:
    let subgoals = [ g | EMeta g<-drop hideCnt args]
    in --trace ("subgoals "++show subgoals) $
       mapM_ mkgoalG subgoals
  where
    mkgoalG g =
      abstractAllG g >>= \ (_,g') ->
      postG g'

    giveApp g name =
      do s <- refineExactG g (EVar name)
         e <- lookupG s g
 	 return (reverse $ args (recordSubstMeta s e))

    args (EApp f x) = x:args f
    args e = []

buildApp n name = app (EVar name) (replicate n m0)

buildDecl names = buildDecl' [(name,[]) | name<-names]

buildDecl' = b
  where
    b names = decl' (map d (buildLHSs names))
      where d (x,ctx) = defA (Value (x,(ctx,m0,m0)))

m0 = ePreMetaVar
m0s = repeat m0

isNDGoal (NDGoal _)= True
isNDGoal _ = False
proofGoalRule (NDGoal r) = Just r
proofGoalRule _ = Nothing

buildPostulate names =
    decl' (map d (buildLHSs names))
  where
    d (x,ctx) = defA (Axiom (x,(ctx,m0)))

buildPackage names =
    decl' (map d (buildLHSs names))
  where
    d (x,ctx) = defA (Package (x,(ctx,PackageDef [])))

refineG dOpts =
    if DO.simpleRefine dOpts
    then refineExactG
    else refineSmartG

--

quickmenuFlag = argFlag "quickmenu" False
tsttermcnt = argReadKey "tsttermcnt" 100 :: Int
cmdprefix = argKey "cmdprefix" ""
