module OldGFPlugin where
import AlfaPlugin
import AlfSyntax
import AlfaText
import UAbstract
import UAnnots hiding (prAnnot)
import AbstractOps(changeDeclDetail,mapModuleDecls)

import Char(isSpace,isDigit)
import Maybe(mapMaybe,listToMaybe)
import Array

import ListUtil(chopList,breakAt)

import Debug2(trace)

import GFAlfaLib

myname = "OldGF"

type LangState = (Grammar,[String]) -- current grammar, local annotations
type GFState = Array Language LangState

plugin =
  Methods {
    name = myname,
    state0 = return (startState::GFState),
    import' = gfloadModule True, -- !! hmm
    load = gfloadModule False,  -- !!! error handling is missing
    save = addDummy . saveAnnots,
    describe = descr,
    altdisp = \ st -> [(langName l,showAsLang l st)|l<-languages],
    altparse = \ st -> [(langName l,parseLang l st)|l<-languages]
  }
  where
    startState = array langRange [(l,(g,[])) | (l,g)<-baseGrammars]
    saveAnnots state = [show l++" "++a|(l,(_,as))<-assocs state,a<-as]

    addDummy [] = [""]
    addDummy annots = annots

    descr st sy =
      case sy of
        ModuleS (Module []) -> (Just (myname ++ " plugin is awake!"),[])
	ExpS e ->
          case e of
	    EMeta m -> (Nothing,giveCmds st)
	    EAnnot (AsTextBy _) _ -> (Nothing,[])
	    _ -> (Nothing,shCmds expEng e)
        DeclS d -> (Nothing,shCmds declEng d++changeLinCmds st d)
        DeclsS ds -> (Nothing,shCmds (map . declEng) ds)
	ModuleS m -> (Nothing,shCmds (mapModuleDecls . declEng) m)
        _ -> (Nothing,[])

    giveCmds st = [giveLangCmd l st|l<-indices st]
    giveLangCmd l st = (cmdtxt,giveEng)
      where
        giveEng = EditWithText cmdtxt "" (parseLang l st) Give
        cmdtxt = myname++": Give in "++langName l

    parseLang l st s =
	case parseExpr grcf s of
	  [e] -> Right e
	  e:es -> trace (show (length es+1)++" possible parses, using the first") $
		  Right e
	  _ -> Left ((0,0),"syntax error")
      where
        (gr,_) = st!l
	grcf = grammar2grammars gr

    shCmds f s = map (shLangCmd f s) languages
    shLangCmd f change l = (myname++": Show in "++langName l,
                            Replace (syn (f l change)))
    expEng = EAnnot . asTextBy . fname
    declEng = changeDeclDetail . Just . DeclAsTextBy . fname
    fname l = (myname,langName l)

    showAsLang l st s =
      case s of
        ExpS e -> atext $ prprA (lin gr e)
	DefBS d -> --trace ("showAsLang "++show d++"\n"++prTerm (ggf gr d)) $
	           atext $ prprA (lin gr d)
	_ -> [(0,[TPlain (typeof s)])]
      where
        (gr,_) = st!l

    changeLinCmds st d = [changeLinCmd l st d n|n<-namesDecl d,l<-languages]
    changeLinCmd l st d n@(Var s) =
        (cmdtxt,changeLin)
      where
        (gr,annots) = st!l
        cmdtxt = myname++": Change "++langName l++" translation of "++s
        changeLin = EditWithText cmdtxt def parse (ReplaceState . loadit def)

        def = case filter ((==s).annotfor) annots of
	        a:_ -> a
		_ ->
		  case lookup s (map a2gD $ decls2defs [d]) of
		    Just t -> prAnnot gr (s,(t,defaultEntry gr s t))
		    _ -> "" -- shouldn't happen

        parse annot =
	  case pAATheory gr [d] [annot] of
	    (_,_:_) -> Right annot -- Returns input string, parser output ignored!
	    _ -> Left ((0,0),"syntax error")

        loadit def annot =
	    if words def == words annot -- quick hack
	    then st -- no change
	    else st//[(l,gfloadLangAnnots False (gr0,annots0) [d] [annot])]
	  where
	    annots0 = filter ((/=s) . annotfor) annots
	    gr0 :: Grammar
	    -- ((Tokenizer, [(Ident, [Tag])], [(Ident, OpDef)]), ([(Ident, CatDef)], [VarSpec], [(Ident, Rule)], [Definition]))
	    gr0 = case gr of
	            (x,(y,w,rules,z)) ->
		      (x,(y,w,filter ((/=s).fst) rules,z))

        annotfor = fst.head.lex

---
gfloadModule imported st0 (Module decls) =
    gfloadAnnots imported st0 decls . mapMaybe annotLang
  where
    annotLang a = listToMaybe [(l,r)|(s,r)<-lex a,(l,_)<-reads s]
     -- Annotations for unknown languages are silently discareded!!!

gfloadAnnots :: Bool -> GFState -> Decls -> [(Language,String)] -> GFState
gfloadAnnots imported st decls annots =
  array langRange
    [(l,gfloadLangAnnots imported st0 decls (langAnnots l))|
     (l,st0)<-assocs st]
  where
    langAnnots l = [a|(l',a)<-annots,l'==l]

gfloadLangAnnots :: Bool -> LangState -> Decls -> [String] -> LangState
gfloadLangAnnots imported (gr0,annots0) decls annots =
  --trace ("gfload "++show annots) $
  (chk $ a2gG gr0 $ a2aatheory gr0 $ pAATheory gr0 decls annots',
   annots0++modannots)
  where
    modannots = if imported then [] else annots'
    annots' = filter (not . null) $ map trim annots
    trim = reverse . trim1 . reverse . trim1
    trim1 = dropWhile isSpace

    chk gr = trace (checkGrammar gr++"\n"++prGrammar gr) gr

--atext' gt = trace (show gt++"\n"++show at) at where at = atext gt

atext = map apara . chopList (breakAt ANewline) . filter (/=AString "")
  where
    apara ws =
      case span (==AIndent) ws of
	(is,ws) -> (2*length is,concatMap aword ws)

    aword (AString s) = concatMap tword (words s)
    aword (AMeta m) = [TMeta m]
    aword _ = []

    -- A quick hack for variable names...
    tword "" = []
    tword s0@('{':s@(_:_)) =
      case break (=='}') s of
        (s1,'}':s2) -> TVar s1:tword s2
        _ -> [TPlain s0]
    tword s = [TPlain s]

--decls2defs decls = [def|Decl _ defs<-decls,DefA _ def<-defs]
