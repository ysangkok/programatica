module PescaPlugin where
import AlfaPluginKit --as A
import qualified PluginPesca as P

import Debug2(badtrace)

myname = "Pesca"

plugin =
  Methods {
    name = myname,
    state0 = const $ return (), -- no state
    loadDecls = dummyLoad,
    save = const [],
    describe = descr,
    altdisp = altdispfs,
    altparse = const [],
    fileparsers = [], -- no specific file formats
  }
  where
    altdispfs st = []
    dummyLoad imported s ds as = Right s
    descr st menv (sy:_) =
	case sy of
	  ModuleS m -> (Just (myname ++ " plugin is awake!"),[])
	  ExpS e ->
	      case e of
		EMeta m -> (Nothing,giveCmds m)
		EAnnot (AsTextBy _) _ -> (Nothing,[])
		EAnnot (AltViewsBy _) _ -> (Nothing,[])
		_ -> (Nothing,[])
	  DeclS (Comment _) -> (Nothing,[])
	  DeclS d -> (Nothing,[])
	  DeclsS ds -> (Nothing,[])
	  --VarS n@(Var s) -> (Nothing,changeLinCmds n st)
	  --ConS n@(Con s) ->(Nothing,changeConLinCmds n st)
	  _ -> (Nothing,[])
      where

	giveCmds m = [parsePropCmd,giveLangCmd]
	  where
	    giveLangCmd = (cmdtxt,giveEng)
	      where
	        giveEng = EditWith editargs giveOrMenu
		editargs = 
                   (stringEdit cmdtxt (P.tryProveExp st t)){complete=compl}
		(_,t) = menv m
		cmdtxt = myname ++ ": Try Pesca"

	        giveOrMenu []  = Message (Left "no parse") -- Shouldn't happen
		giveOrMenu [e] = Give e
		giveOrMenu es  = Menu [(Give e,("Parsed",syn e))|e<-es]


		compl s = []
	    parsePropCmd = (cmdtxt,giveEng)
	      where
	        giveEng = EditWith editargs giveOrMenu
		editargs = 
                   (stringEdit cmdtxt (P.parseProposition st)){complete=compl}
		(_,t) = menv m
		cmdtxt = myname ++ ": Parse proposition"

	        giveOrMenu []  = Message (Left "no parse") -- Shouldn't happen
		giveOrMenu [e] = Give e
		giveOrMenu es  = Menu [(Give e,("Parsed",syn e))|e<-es]

		compl s = []

	shCmds f s = []
