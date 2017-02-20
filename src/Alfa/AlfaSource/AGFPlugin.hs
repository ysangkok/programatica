module AGFPlugin(plugin) where
import AlfaPluginKit --as A
import FilePathUtils(abschildpath)
import qualified AGF as GF
-- #ifdef __HBC__
--import Char(decodeUTF8)
-- #endif
import FudUTF8(decodeUTF8)

import Char(isSpace)
import Maybe(fromMaybe)
import List(partition)
import ListUtil(chopList,breakAt)
--import Array

--import ListUtil(chopList,breakAt)

import Debug2(badtrace,trace)

myname = "AGF"

type AGFState = (GF.State,(TodoList,DG))
type TodoList = [(FilePath,GF.AGFGrammar)] -- parsed grammar to be imported
type DG = [(FilePath,[FilePath])] -- (reverse) import dependency graph

plugin =
  Methods {
    name = myname,
    state0 = \ libs -> do st <- GF.initAGF -- Any need to propagate libDirs?
                          return (st,([],[])),
    loadDecls = loadGrammar,
    save = const [],
    describe = descr,
    altdisp = altdispfs,
    altparse = \ st -> [], --[(langName lang,GF.parseExp st lang)|lang<-GF.languages st],
    fileparsers = [(n,parsefile n) | n <- words "gfu gfn gf ebnf gfo cf xml"]
  }
  where
    altdispfs (st,_) =
        --("All",dispall):
        [(langName lang,disp lang)|lang<-GF.languages st]
      where
    	disp lang menv s =
      	  fromMaybe (plaintext (typeof s)) (GF.linearize st lang menv s)
	{-
	dispall menv s =
	  case s of
	    ExpS e -> PlainPara [TSyntax e] :
		      [NestedPara txt | Just txt <-
		       [GF.linearize st lang menv s|lang<-GF.languages st]]
	    _ -> plaintext (typeof s) -- hmm
	-}

    loadGrammar dsrc s@(agf,(todo,dg)) ds as =
	case dsrc of
	  Imported filepath -> imp filepath
	  Opened filepath -> imp filepath
	  _ -> Right s -- ignore appended declarations
      where
        imp filepath =
	  case partition ((==filepath).fst) (todo::TodoList) of
	    ([(_,gr)],todo') ->
	        do let ls = grLangs dg filepath
		   agf' <- trace ("GF.importGrammar "++filepath++show ls++show dg) $
			   GF.importGrammar agf filepath ls gr
		   return (agf',(todo',dg))
	    ([],todo) -> Right s --err (filepath++"wasn't parsed by GF...")
	    _ -> err "Something strange is going on with grammar imports..."
	err s = Left (unknownSourcePos,s)

    parsefile ext (st,(todo,dg)) imported filename str =
        do (mod,gr) <- GF.parseGrammar ext (filename,str')
	   let dg' = foldr (addimport filename) dg (fst gr)
	   return (mod,(st,((filename,gr):todo,dg')))
      where
        str' = if ext=="gfu"
-- #ifdef __HBC__
	       then decodeUTF8 str
-- #else
--               then badtrace "No unicode support!!" str -- !!!
-- #endif
	       else str

        addimport from relf' dg0 =
	    case dg0 of
	      [] -> [(f',[from])]
	      (d@(f,fs):dg) ->
		if f'==f && from `notElem` fs
		then (f,from:fs):dg
		else d:addimport from f' dg
	  where
	    f' = abschildpath from relf'

    descr (st,(todo,_)) menv (sy:_) =
	case sy of
	  ModuleS m -> (Just (myname ++ " plugin is awake! " ++
			      show(length todo)),
	                shCmds (mapModuleDecls . declEng) m)
	  ExpS e ->
	      case e of
		EMeta m -> (Nothing,giveCmds m++shcmds)
		EAnnot (AsTextBy _) _ -> (Nothing,[])
		EAnnot (AltViewsBy _) _ -> (Nothing,[])
		_ -> (Nothing,shcmds)
	    where shcmds = shExpAllCmd e:shCmds expEng e
	  DeclS (Comment _) -> (Nothing,[])
	  DeclS d -> (Nothing,shCmds declEng d)
	  DeclsS ds -> (Nothing,shCmds (map . declEng) ds)
	  --VarS n@(Var s) -> (Nothing,changeLinCmds n st)
	  --ConS n@(Con s) ->(Nothing,changeConLinCmds n st)
	  _ -> (Nothing,[])
      where
        languages = GF.languages st

	giveCmds m = [giveLangCmd l|l<-languages]
	  where
	    giveLangCmd l = (cmdtxt,giveEng)
	      where
	        giveEng = EditWith editargs giveOrMenu
		editargs = (stringEdit cmdtxt (GF.parseExp st l t)){complete=compl}
		(_,t) = menv m
		cmdtxt = myname++": Give in "++langName l

	        giveOrMenu []  = Message (Left "no parse") -- Shouldn't happen
		giveOrMenu [e] = Give e
		giveOrMenu es  = Menu [(Give e,("Parsed",syn e))|e<-es]

		dict = GF.wordsOfLang l st

		compl s =
		    [(dw,compl) | dw<-dict,
				  let (pre,compl)=splitAt n dw,
				  pre==w]
		  where
		    w = reverse . takeWhile (not . isSpace) . reverse $ s
		    n = length w

	shCmds f s = map (shLangCmd f s) languages
	shLangCmd f change l = (myname++": Show in "++langName l,
				Replace (syn (f l change)))
	expEng = EAnnot . asTextBy . fname
	declEng = changeDeclDetail . Just . DeclAsTextBy . fname
	fname l = (myname,langName l)

        shExpAllCmd e =
	    (myname++": Show all translations",
	     Replace (syn (EAnnot (altViewsBy views) e)))
	  where
	    views = [(myname,langName l)|l<-languages]


langName = GF.langName

grLangs dg filename = maybe parentLangs (:[]) (nameLang filename)
  where
    parentLangs = concatMap (grLangs dg) (fromMaybe [] $ lookup filename dg)

    nameLang filename =
	case chopList (breakAt '.') . takeWhile (/='/') . reverse $ filename of
	  rext:rlang:_:_ -> Just (reverse rlang)
	  _ -> Nothing

-- This doesn't belong here:

instance Monad (Either e) where
  return = Right
  Left e >>= _ = Left e
  Right x >>= f = f x
