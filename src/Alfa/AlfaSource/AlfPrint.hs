{-# COMPILERFLAGS -fno-overload-restr #-}
module AlfPrint(prSyntax,prSyntax',prSyntax1line) where
--import AlfAbstract
import UAbstract
import UAnnots
import AbstractOps(flatApp)
--import AlfModules(Module(..),Import(..),CDecl(..),FileName(..))
import AlfSyntax
--import AlfParse(lexanal,TOKEN(..))
import NewPP
import Fudgets(argReadKey)
import DrawOptions(declDetail0)
import AlfaText
import Maybe(fromMaybe)

-- Imagine how nice this would be with proper record types...
prSyntax = prSyntax' declDetail0 []
prSyntax' declDetail altdispfs = pr declDetail altdispfs . syn

prSyntax1line = unwords . words . prSyntax

prettyw = argReadKey "prettyw" 72
prettyr = argReadKey "prettyr" 60

pr declDetail altdispfs = pretty prettyw prettyr . prSyntax
  where
    prSyntax syntax =
      case syntax of
--	ImportsS is -> prImports is
	ImportS i -> prImport i
--	NameS n -> prName n
	ExpS e -> prExp e
	DeclS d -> prDecl d
	DeclsS ds -> prDecls ds
--	CDeclS d -> prCDecl d
--	CDeclsS ds -> prCDecls ds
	ModuleS m -> prModule m
--	AlfContextS (Ctx _ ctx) -> prCtx ctx
	BranchS b -> prBranch b
	BranchesS bs -> prBranches bs
--	AlfConstructorS (AlfCon c) -> prCon c
--	AlfConstructorsS cs -> prCons (map unAlfCon cs)
	TypingS nt -> prTyping nt
--	TypingsS nt -> ...
	_ -> text (typeof syntax)

    prImports = vert . map prImport
    prImport (Import filename) = text ("#include \""++filename++"\"")

    prExp = prExp' id
    prAExp = prExp' paren

    prExp' paren' e =
      case e of
	EApp e1 e2 -> paren' (prAExp f <+> sep (map prAExp as))
	  where (f,as) = flatApp e1 e2
	EVar i -> prVar i
        ECon c -> prCon c
--	ECon c es -> paren' (sep (prCon c:map (nest 2 . prAExp) es))
--	ELCon c es -> paren' (sep (prConst c:map (nest 2 . prAExp) es))
	EAbs (n :- _) e ->
	  paren' (sep [horiz' [text "\\",prVar n,text "->"],prExp e])
	ELet ds e -> paren' $ compound "let" (prDecls ds) $$
	                       (text "in "<>prExp e)
	ECase e bs ->
	  paren' $ sep [sep [text "case",nest 2 (prExp e),text "of {"],
		        nest 2 (prBranches bs),
			text "}"]

	EPi narg res -> paren' $ sep [paren (prTyping narg)<>text "->",prExp res]
	ESort s -> prSort s
	ESum sum -> paren' $ prSum sum

	EProj e n -> paren' $ prAExp e<>text "."<>prSel n
	ESig sig -> paren' $ compound "sig" (prSig sig)
	EStr str -> paren' $ compound "struct" (prStr str)
	--ECom com -> prCom com
	EMeta m -> prMeta m
	EAnnot a@(AsTextBy (N fn)) e' ->
	    case lookup fn altdispfs of
	      Just dr -> prText (dr (syn e'))
	      Nothing -> prExp' paren' e'
	EAnnot a@(AltViewsBy (N fns)) e' ->
	    vert (prExp e':
		    [prText (dr (syn e'))
		     | Just dr<-map (flip lookup altdispfs) fns])
	EAnnot a@(EComment s) e -> paren' $ prComment' s <> prExp e
	EAnnot a@Hidden e' -> text "..."
	EAnnot a e -> prExp' paren' e

    prSum = compound "data" . prConstrs
    prConstrs = safesep . sep' (text "|") . map prConstr
    prConstr (Constr (c,(args,cmnt))) =
        horiz' [prCon c,prCtx args,prEqRestr cmnt]

    prStr = prDecls
    prSig = safesep . semisep . map prSigPart
      where
        prSigPart (SigField lbl e) = horiz' [prLabel lbl,text ":",prExp e]
	prSigPart (SigDef defb) = prDefBstd defb

    prBinds s = safesep . commasep . map (prBind' s)
    prBind' sep (n,e) = horiz' [prVar n,text sep,prExp e]
    prBind = prBind' ":"

    prTyping (x:-t) = prBind (x,t)

    prModule (Module decls) = prDecls decls

    prDecls [] = text ""
    prDecls (Comment s:cds) = prComment' s $$ prDecls cds
    prDecls (ImportDecl _:cds) = prDecls cds -- !!
    prDecls (Decl _ ds:cds) =
        text "" $$ 
	(prDefs ds<>optcomma) $$
	prDecls cds
      where optcomma =
	      case [d | Decl _ d <- cds] of
                [] -> text ""
		_  -> text ";"

    prDecl (Comment s) = prComment' s
    prDecl (Decl _ defs) = prDefs defs
    prDecl (ImportDecl i) = text "" -- !!
    prComment' s = text ("{-"++s++"-}")

    prDefs = safesep . semisep . map prDef
      where
        prDef (DefA ps@(_,props,optDetails) defb) = prDefB optDetails defb -- !!

    prDefB optDetails defb =
      case fromMaybe declDetail optDetails of
	DeclAsTextBy fn ->
	  case lookup fn altdispfs of
	    Just dr -> prText (dr (syn defb))
	    _ -> std
	DeclAltViewsBy fns ->
	    vert (std:[prText (dr (syn defb))
		       | Just dr<-map (flip lookup altdispfs) fns])
        _ -> std
      where std = prDefBstd defb

    prDefBstd defb =
      case defb of
        CommentDef s -> prComment' s
        Package (n,(ctx,body)) ->
	    sep [prLhs' "package" n ctx,nest 2 $ prPackageBody body]
	  where
            prPackageBody b =
	      case b of
                PackageDef ds -> compound "where" (prDecls ds)
		PackageInst e -> text "=" <+> prExp e
	Open (e,oas) -> sep [text "open" <+> prExp e,
			     nest 2 $ text "use" <+> prOpenArgs oas]
	  where
	    prOpenArgs = horiz' . commasep . map prOpenArg

	    prOpenArg (n,(optt,optas)) = horiz' [optasD, prVar n,opttD]
	      where
		opttD = case optt of
			  Nothing -> text ""
			  Just t -> text "::"<+>prExp t
		optasD = case optas of
			   Nothing -> text ""
			   Just n2 -> prVar n2<+>text "="
	Data (n,(ctx,sum)) ->
	    sep [prLhs' "data" n ctx,
		 nest 2 $ text "=" <+> brace (prConstrs sum)]
	Type (n,(ctx,e)) ->
	    sep [prLhs' "type" n ctx,
		 nest 2 $ text "=" <+> prExp e]
	Axiom (n,(ctx,t)) ->
	    sep [prLhs' "postulate" n ctx,
		 nest 2 $ text "::" <+> prExp t]
        Binding (n,e) -> sep [horiz [prVar n,text "="],nest 2 (prExp e)]
	Value (en,(ctx@(Ctx l ps),eval,etype)) ->
	    sep [prLhs en ctx,
		 nest 2 $ text "::" <+> prExp etype,
		 nest 2 $ text "=" <+> prExp eval]
        _ -> text "{-unimplemented def-}"

    prLhs n ctx = prVar n <> prCtx ctx
    prLhs' k n ctx = text k <+> prLhs n ctx

    prCtx (Ctx _ ps) = prCtx' ps

    prCtx' []  = text ""
    prCtx' ctx = paren . prBinds ":" $ ctx

    prBranches bs = vert (semisep (map prBranch bs))
    prBranch (Branch (n,(ns,e))) =
	sep [paren (horiz' (prCon n:map prVar ns))<+>text "->",nest 2 (prExp e)]

    prEqRestr [] = text ""
    prEqRestr cmnt = angleBrack (safesep (commasep (map prEqn cmnt)))
    prEqn (e1,e2) = sep [prExp e1 <> text "=", nest 3 (prExp e2)]

    prText = vert . (text "":) . map prPara
      where
	prPara (PlainPara items) = prItems items
	prPara (NestedPara text) = nest 2 (prText text)

	prItems = horiz' . map prItem
	prItem item =
	  case item of
	    TSyntax stx -> prExp stx
	    TPlain s -> text s
	    TSymbol optfont s -> text s -- !!!
	    TVar s -> text s -- !!

vert [] = text ""
vert ds = foldr1 ($$) ds

paren = inside "(" ")"
brace = inside "{" "}"
--brack = inside "[" "]"
angleBrack = inside "<" ">"
inside l r doc = text l <> doc <> text r

prSort (Sort s) = text s
--prName (Name s) = prIden s
prVar (Var s) = prIden s
prCon (Con s) = prIden s
prLabel (Label s) = prIden s
--prConst = prName
--prIdent = prName

prMeta m = text "?" -- ++show (m::Int)

prIden = text . quoteIden

quoteIden s = s {-
  case lexanal s of
    [(_,Iden _),eot] -> s
    _ -> "\""++s++"\""
-}

prSel (Label s) = text q {-
    case lexanal ('.':q) of
      [(_,Symbol "."),(_,Iden _),eot] -> text q
      _ -> text (' ':q)
-}
  where q = quoteIden s

sepwith s = sep . sep' s

commasep = sep' (text ",")
semisep = sep' (text ";")

sep' s [] = []
sep' s [d] = [d]
sep' s (d:ds) = (d<>s):sep' s ds

safesep [] = text ""
safesep ds = sep ds

horiz = foldr (<>) (text "")

horiz' [] = text ""
horiz' ds = foldr1 (<+>) ds

d1 <+> d2 = d1 <> text " " <> d2

compound kw d = sep [text (kw++" {"), nest 2 d,text "}"]
