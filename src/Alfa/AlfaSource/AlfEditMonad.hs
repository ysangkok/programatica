module AlfEditMonad where
import EditMonad
import Fud(replace)
import AlfState(AlfState,currentFile,
  --proofState,setProofState,
  setCurrentState,currentState,changePluginState,pluginState,setPluginState,
  savePlugins,
  csps,setCsPs,loadAlfState,ImportState(..),importState,setImportState)
--import ProofmonadOps(steps,G(..))
import qualified ProofEngine as PE
import qualified Plugins as Plug
--import AlfModules(FileName(..))
import UAbstract(Module(..)) -- + instances
import AlfaPlugin() -- type synonyms

#ifdef __HASKELL98__
#define map fmap
#endif

type AlfEdM a = EdM AlfState String a

--edPE :: PE.PE a -> AlfEdM a
edPE = liftStateEd csps setCsPs

appendDeclsEd decls0 = 
  do (decls,_) <- edPE (PE.appendDecls decls0)
     appendDeclsPluginsEd decls
     return decls

loadAlfStateEd filename imported mod annots =
  do state <- loadEd
     let (newstate,badannots) = loadAlfState filename imported mod annots state
     storeEd newstate
     return badannots

importStateEd = mapEd importState loadEd
setImportStateEd = updateEd . setImportState

dropThisModuleEd =
  do is <- importStateEd
     a <- edPE PE.getAutoSolve
     updateEd (setCurrentState (case is of
			      --[] -> alfstate0
			        (csps,_):_ -> csps))
     edPE $ PE.setAutoSolve a
     -- drawOptions ??!!

moveToImportsEd filename =
  do is <- importStateEd
     st <- loadEd
     setImportStateEd ((currentState st,filename):is)

isImportedEd filename = mapEd (elem filename.map snd) importStateEd

savePluginsEd :: AlfEdM [(Plug.PluginName,Plug.Annotations)]   
savePluginsEd = savePlugins `map` loadEd

reloadPluginsEd :: Module -> [(Plug.PluginName, Plug.Annotations)] -> AlfEdM ()
reloadPluginsEd mod savedPlugs =
    do filename <- fmap currentFile loadEd
       updateEd $ changePluginState $ zipWith (reloadPlug filename) savedPlugs
  where
    reloadPlug filename (name',annots) (name,Plug.Plugin st m) | name==name' =
        case Plug.loadModule m (Plug.Opened filename) st mod annots' of
          Right st' -> (name,Plug.Plugin st' m)
        --Left err -> -- may happen for buggy plugins
      where annots' = map ((,) Plug.unknownSourcePos) annots -- hmm

tryChangePluginsEd upd =
  liftStateEd pluginState setPluginState $
  loadEd >>= mapM (liftEitherEd . upd) >>= storeEd

appendDeclsPluginsEd decls = tryChangePluginsEd appendDecl
  where
    appendDecl (name,Plug.Plugin st m) =
      case Plug.loadModule m Plug.Appended st (Module decls) [] of
        Right st' -> Right (name,Plug.Plugin st' m)
	Left (pos,err) -> Left err

updatePluginEd plugin =
  liftStateEd pluginState setPluginState $
  updateEd (replace plugin)
