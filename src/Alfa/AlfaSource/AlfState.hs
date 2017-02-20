module AlfState(
  AlfState,currentFile,setCurrentFile,ImportState(..),
  csps,setCsPs,--changeCsPs,
  currentState,setCurrentState,changePluginState,pluginState,setPluginState,
  --proofState,setProofState,changeProofState,
  --checkState,
  --theory,psTheory,
  drawOptions,changeOptions,
  importState,setImportState,
  pluginVar,pluginPrinters,pluginParsers,altdispfs,metaEnv,metaVars,altparserfs,
  PluginName,DispFunName,
  menuLangVar,MenuLang(..),
  alfHiding,alfCompact,setAlfHiding,lookupAlfArgOpts,
  alfUnfoldGoals,alfAutoAbstract,alfOnlyRefine,alfSimpleRefine,
  alfLayoutStyle,LayoutDirection(..),
  alfDeclDetail,DeclDetail(..),
  alfProofStyle,ProofStyle(..),
  alfHideTrivialLet,
  alfFontSize,alfAutoSolve,
  {-
  getAlfHiding,getAlfCompact,getAlfUnfoldGoals,getAlfLayoutStyle,
  getAlfProofStyle,getAlfFontSize,getAlfDeclDetail,
  getAlfHideTrivialLet,
  getAlfAutoSolve,
  -}
  initState,
  resetState,
  prAlfState,loadAlfState,
  savePlugins,
) where
--import AlfAbstract
--import AlfModules(FileName(..))
import UAbstract(ePreMetaVar)
import Fonts(AlfFonts)
import DrawOptions
import DrawOptionsPrinter
import DrawOptionsParser
import AlfaMenuBar(MenuLang(..),menuLang0)
import GeditOptions(GEditInputMode(..))
--import ProofmonadOps
import qualified ProofEngine as PE
import Plugins(Plugins,Plugin(..),PluginName,initPlugins,showPluginStates)
import qualified AlfaPlugin as Plug

import Maybe(mapMaybe)
import List(partition,mapAccumL)
import HO(apSnd)
import ListUtil(mapFst,mapSnd)
import ListMap(add)
import Char(isSpace)
import Fud(argKeyList)

import Variables
import Collect(collectByFst)

import Debug2(trace,badtrace)
#ifdef __HASKELL98__
#define map fmap
#endif

#define SET(f) (\ v r -> r { f=v })
#define FieldVar(f) (Vari { set=SET(f), get=f })

type CsPs = PE.State
type ImportState = [((CsPs,Plugins),FilePath)]

data AlfState
  = AlfState {
      currentFile::FilePath,
      csps::CsPs,
      pluginState::Plugins,
      importState::ImportState,
      menuLang::MenuLang,
      drawOptions::DrawOptions
    }

initState libDirs =
  do activePlugins <- activatePlugins libDirs
     let is0 = [((csps0,activePlugins),"")]
     return (\ filename -> AlfState filename csps0 activePlugins is0 menuLang0 . drawOptions0)
  where
    csps0 = PE.initState

currentState AlfState {csps=csps,pluginState=plugs} = (csps,plugs)

--resetState = state0 filename.fontsOpt.drawOptions $ state
--  where filename = currentFile state

resetState state =
    state{csps=csps0,
          pluginState=plugs0,
	  importState=[is1],
	  drawOptions=drawOptions0 (fontsOpt.drawOptions $ state)}
  where
    is1@((csps0,plugs0),_) = last (importState state)

pluginVar = FieldVar(pluginState)
menuLangVar = FieldVar(menuLang)

setCsPs csps' s = s { csps = csps' }
changeCsPs f s @ (AlfState { csps=csps }) = s {csps = f csps}
--setProofState ps s @ (AlfState { csps = (cs,_) }) = s { csps=(cs,ps) }
--changeProofState f = changeCsPs (apSnd f)

setImportState is s = s { importState = is }
setCurrentState (csps,plugs) state = state{csps=csps,pluginState=plugs}
changePluginState f s@AlfState{pluginState=p} = s{pluginState=f p}
setPluginState p = changePluginState (const p)

--checkState = fst.csps
--proofState = snd.csps
changeOptions f s @ (AlfState {drawOptions=dOpts}) = s {drawOptions=f dOpts}

optionVar = liftState drawOptions (\ o a -> a {drawOptions=o})


peVar ::(a->PE.PE ()) -> PE.PE a -> Variable AlfState a
peVar setPE getPE =
  Vari { get = flip PE.inspect getPE . csps,
	 set = changeCsPs . flip PE.changestate . setPE }

alfHiding = optionVar hidingOnOpt
alfCompact = optionVar compactOnOpt
alfUnfoldGoals = optionVar unfoldGoalsOpt
alfAutoAbstract = optionVar autoAbstractOpt
alfOnlyRefine = optionVar onlyRefineOpt
alfSimpleRefine = optionVar simpleRefineOpt
alfAutoSolve = peVar PE.setAutoSolve PE.getAutoSolve
alfLayoutStyle = optionVar layoutStyleOpt
alfDeclDetail = optionVar declDetailOpt
alfProofStyle = optionVar proofStyleOpt
alfHideTrivialLet = optionVar hideTrivialLetOpt
alfFontSize = optionVar fontSizeOpt
setAlfHiding = changeOptions . setHiding

lookupAlfArgOpts x = lookupArgOpts . drawOptions $ x
--lookupAlfHiding = hideCnt `oo` lookupAlfArgOpts

setCurrentFile f s = s{currentFile=f}

activatePlugins libDirs =
  initPlugins libDirs $
  case argKeyList "plugins" [] of
    ["yes"] -> const True
    names   -> (`elem` names)

altdispfs state =
  [((pn,fn),f (metaEnv state)) | 
   (pn,Plugin pstate p)<-pluginState state,
   (fn,f) <- Plug.altdisp p pstate]

metaEnv state m =
  case PE.query (csps state) (PE.goal 0 m) of
    Right env_t -> env_t
    Left err -> ([],ePreMetaVar) -- !!

metaVars state = PE.inspect (csps state) PE.metaVars

pluginPrinters state =
  [(DeclAsTextBy (pn,fn),pn++": "++fn) |
   (pn,Plugin pstate m)<-pluginState state,
   (fn,_)<-Plug.altdisp m pstate]

altparserfs state =
  [((pn,fn),f) | 
   (pn,Plugin pstate p)<-pluginState state,
   (fn,f) <- Plug.altparse p pstate]

pluginParsers state =
  [(ParsedInput (pun,pan),pun++": "++pan) |
   (pun,Plugin pstate m)<-pluginState state,
   (pan,_)<-Plug.altparse m pstate]

prAlfState alfstate =
    concatMap prAnnot $
    ("Alfa",printDrawOptions.drawOptions $ alfstate) :
    [(name,annot)|(name,annots)<-savedplugs,annot<-annots]
  where
    savedplugs = savePlugins alfstate

prAnnot (name,str) = "{-# "++name++" "++str++" #-}\n"

savePlugins = mapSnd save' . pluginState
  where save' (Plugin st m) = Plug.save m st

data AnnotType
  = UnknownAnnot
  | OldLayout
  | AlfaLayout
  | PluginAnnot String
  deriving (Eq)

loadAlfState filename imported mod annots alfstate =
    --trace (showPluginStates (pluginState newstate)) $
    apSnd concat new
  where
    new = mapAccumL load alfstate .
	  addRemainingPlugins .
	  collectByFst .
	  map identify $ annots
      where
        load state annots = 
	  case loadAnnot annots state of
	    Just state' -> (state',[])
	    Nothing -> (state,prAnnots annots)

        prAnnots (t,annots) = map (curry prAnnot (prAnnotType t).snd) annots

	prAnnotType AlfaLayout = "Alfa"
	prAnnotType (PluginAnnot name) = name
	prAnnotType _ = ""

        addRemainingPlugins annots=
	    [(PluginAnnot p,[])|(p,_)<-pluginState alfstate,
	                        p `notElem` aps]
	    ++annots
	  where
	    aps = [name|(PluginAnnot name,_)<-annots]

    identify (pos,annot) =
      case lex annot of
	[(tag,rest)] ->
	  case tag of
	    "Alfa" -> (AlfaLayout,(pos,rest))
	    "GlobalArgOptions" -> (OldLayout,(pos,annot))
            _ -> (PluginAnnot tag,(pos,rest))

	_ -> (UnknownAnnot,(pos,annot)) -- unrecognized annotation !!

    loadAnnot (t,pannots) = 
      case t of
	AlfaLayout ->
	  case parseDrawOptions imported (join pannots) of
	    Right ah -> Just . changeOptions ah
	    Left err -> badtrace err fail -- bad syntax for drawing options!
	OldLayout ->
	  trace "Reading old layout option syntax" $
	  case reads(unwords(map backwardcompat990209 (words(join pannots)))) of
	    [(ah,_)] -> Just . alfaOptions (backwardcompat991018 ah)
	    _        -> fail -- bad syntax for drawing options!
	PluginAnnot pn -> updateM pluginVar (updatePlugin pn pannots)
	_ -> fail -- unrecognized annotation!

      where
        join = concatMap snd
        fail = const Nothing

    alfaOptions ah = changeOptions (mergeHiding imported ah)

    updatePlugin name annots plugins =
       case lookup name plugins of
         Just (Plugin state m) ->
	     --trace ("Loading data for plugin "++name++" from "++show annots) $
	     Just (add (name,Plugin state' m) plugins)
	   where
	     state' =
	       case Plug.loadModule m dsrc state mod annots' of
	         Right s -> s
		 Left (p,msg) -> badtrace (show p++"\n"++msg) state -- !!!
	     annots' = mapFst Plug.sourcePos annots
	     --state' = trace ("Load: "++show (Plug.save m state'')) state''
	 _ -> --trace ("No plugin: "++name) -- unrecognized annotation !
	      Nothing

    dsrc = if imported then Plug.Imported filename else Plug.Opened filename

backwardcompat990209 w =
  case w of
   "Quantifier," -> "Quantifier True,"
   _ -> w

backwardcompat991018 gaopts =
    gaopts { argOptions = concatMap n2vc (argOptions gaopts) }
  where
    n2vc (Name s,opts) = [(Var s,opts),(Con s,opts)]
