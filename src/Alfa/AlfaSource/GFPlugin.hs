module GFPlugin where
import AlfaPluginKit --as A
import qualified GFforAlfa as GF
import Fud(argKeyList)

import Monad -- mplus
import Char(isSpace)
import Maybe(fromMaybe)
import List(isPrefixOf)
--import Array

--import ListUtil(chopList,breakAt)

--import Debug2(trace)

myname = "GF"

plugin =
  Methods {
    name = myname,
    state0 = GF.initGF langs . map (++('/':myname)),
    loadDecls = ifImported GF.importModule GF.loadModule,
    save = {-addDummy .-} GF.saveAnnotations,
    describe = descr,
    altdisp = \ st -> [(GF.langName lang,disp st lang)|lang<-GF.languages st],
    altparse = \ st -> [], -- [(GF.langName lang,GF.parseExp st lang)|lang<-GF.languages st],
    fileparsers = []
  }
  where
    --addDummy [] = [""]
    --addDummy annots = annots

    disp st lang menv s =
      fromMaybe (plaintext (typeof s)) (GF.linearize st lang s)

    descr st menv (sy:_) =
	case sy of
	  ModuleS m@(Module ds) ->
	    (badAnnots `mplus` Just (myname ++ " plugin is awake!"),
	     shDeclCmds mapModuleDecls m)
	    where
	      badAnnots =
	        case GF.badAnnots st of
		  [] -> Nothing
		  ns -> Just (myname++": bad annotations for: "++unwords ns)

	  ExpS e ->
	    case e of
	      EMeta m -> (Nothing,giveCmds st++shcmds)
	      EAnnot (AsTextBy _) _ -> (Nothing,[])
	      EAnnot (AltViewsBy _) _ -> (Nothing,[])
	      _ -> (Nothing,shcmds)
	    where shcmds = shExpAllCmd e:shCmds expEng e
	  DeclS (Comment _) -> (Nothing,[])
	  DeclS d -> (Nothing,shDeclCmds id d++
		              concatMap (flip changeLinCmds st) (namesDecl d))
	  DeclsS ds -> (Nothing,shDeclCmds map ds)
	  VarS n -> (bad (GF.currentAnnotation n),changeLinCmds n st)
	  ConS n -> (bad (GF.currentAnnotationCon n),changeConLinCmds n st)
	  _ -> (Nothing,[])
      where
        languages = GF.languages st

        bad curr = sh [l|l<-languages,not . null . fst $ curr st l]
	  where
	    sh [] = Nothing
	    sh ls = Just (myname++": bad annotation for languages: "++
	                  unwords (map langName ls))

	giveCmds st = [giveLangCmd l st|l<-languages]
	giveLangCmd l st = (cmdtxt,giveEng)
	  where
	    giveEng = EditWith editargs giveOrMenu
	    editargs = (stringEdit cmdtxt (GF.parseExp st l)){complete=compl}
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

        shDeclCmds f d =
 	  (myname++": Show all translations",Replace (syn (f declAll d))):
	  shCmds (f . declEng) d

        declAll = changeDeclDetail . Just . DeclAltViewsBy $ map fname languages

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

        changeLinCmds n@(Var s) =
 	    changeLinCmds' GF.changeAnnotation GF.currentAnnotation n s
        changeConLinCmds n@(Con s) =
 	    changeLinCmds' GF.changeAnnotationCon GF.currentAnnotationCon n s

    	changeLinCmds' change curr n s st = map changeLinCmd languages
	  where
	    changeLinCmd l = (cmdtxt,changeLin)
	      where
		cmdtxt = myname++": Change "++langName l++" translation of "++s
		changeLin = EditWith editargs ReplaceState
		editargs = (textEdit cmdtxt (change st l n.rmerr)){def=Just def}
		def = showerr (curr n st l)
		showerr (errs,annot) = unlines (map (errprefix++) errs')++annot
		  where errs' = lines . unlines $ errs
		errprefix = "--"++myname++":"
		rmerr = unlines . filter notErrLine . lines
		notErrLine s = not (errprefix `isPrefixOf` s)

langName = GF.langName
langs = argKeyList "langs" GF.supportedLanguages
