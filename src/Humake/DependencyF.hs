module DependencyF(dependencyF,Cmd(..)) where
import Prelude hiding (FilePath,IOError)
import AllFudgets hiding (Placer(..),Spacer(..),Time)
import DependencyGraph
import Modules
import ModuleIds
import FileNames
import HaskellImports
import Project
import TodoGraph
#ifdef __HBC__
#define nilPS nil
#endif
import PackedString(PackedString,packString,unpackPS,nilPS)
import ListMap(ListMap)
import qualified ListMap as LM
import HO
import DialogueIO hiding (IOError)
--import Time
--import Trace(trace)

toModuleList = Left 
toParCompile = Right
putH = putK.High
putsH = putsK.map High
getH k = getK $ message (const $ getH k) k

data State =
  S { project::Project,
      batch::Bool, -- batch compilation, exit when done
      graph::(Graph,TodoGraph),
      linkedTargets::ListMap Target FileDate,
      ids::ModuleIds
    }

dependencyF project ba =
    ioF $ --echoK (show project) $
	  updTargetDatesK LM.empty ltargets $ \ ldates ->
	  let st0 = S project ba (pwtg st0 emptyGraph) ldates ids
	  in updateGraphK st0 rootids
  where (rootids,ids) = moduleIds (roots project) emptyIds
	ltargets = filter (isLinkType.snd.snd) ts
	  where ts = zip rootids (targets project)

updateGraphK st@(S{project=project,graph=graph,ids=ids}) fringe =
  buildGraphK st fringe $ \ graph' ids' ->
  putH (toModuleList (fst graph')) $
  compileK st {graph=graph',ids=ids'}

compileK st@(S{project=project,graph=(graph,todo)}) =
    --echoK (showTodo ids todo) $
    --echoK (show (map fst cmds)) $
    putsH (map toParCompile cmds) $
    --echoK (showTodo ids todo') $
    idleK st{graph=(graph,todo')}
  where
    cmds = map compile compileableMods
    (compileableMods,todo') = whatNow graph todo
    compile ((modid,t),cmd) = tag cmd
      where
	mod = lookupModule graph modid
        tag c = (unpackPS (modName mod),(project,c))

idleK  st@(S{batch=b,graph=g@(graph,todo)}) =
  if b && allDone todo && allOk graph
  then hIOSucc (Exit 0) nullK
  else idleK' st
idleK' st@(S{project=p,graph=g@(graph,_),ids=ids}) =
    getH $ either fromModuleList fromParCompile
  where
    fromModuleList names = updateModules names

    updateModules names =
        --echoK (unwords ("Update ":names)) $
	filterChangedModulesK st nids $ \ nids' ->
	updateGraphK st{graph=pwtg st (removeModules graph nids'),ids=ids'} nids'
      where
        (nids,ids') = moduleIds names ids
	removeModules = foldr removeModule

    fromParCompile (modname,success) =
        if success
	then --echoK ("Compilation of "++modname++" succeeded.") $
             updateDates id ids'
        else --echoK ("Compilation of "++modname++" failed.") $
             updateGraphK st{graph=apFst (inError id) g} []
      where
        (id,ids') = moduleId (packString modname) ids

        updateDates id ids' =
	  checkDatesK (objdir p) (modPath (lookupModule graph id)) $ \ lit dates ->
	  updateGraphK st{graph=mapPair (updModDate id lit dates,doneWith id) g,
			  ids=ids'}
		       []

--filterChangedModulesK st mids cont = cont mids
--{-
filterChangedModulesK st@(S{project=p,graph=graph,ids=ids}) mids cont =
  case mids of
    [] -> cont []
    mid:mids ->
      filterChangedModulesK st mids $ \ mids' ->
      case safeLookupModule (fst graph) mid of
        Nothing -> cont (mid:mids')
	Just mod ->
	  findModule (moduleName mid ids) (srcdirs p) (libdirs p) errK srcK libK
	  where
	    srcK path =
	      checkDatesK (objdir p) path $ \ lit dates ->
	      if path/=modPath mod || -- using a different source file?
	         sourceDate dates > sourceDate (modDates mod)
	      then changed
	      else unchanged
	    libK _ = changed -- no gain by doing like in srcK
	    errK   = changed -- no gain by doing like in srcK
	    changed = cont (mid:mids')
	    unchanged = cont mids'
---}

buildGraphK st@(S{project=p,graph=graph,ids=ids}) fringe cont =
    traverseK fringe graph ids
  where
    traverseK fringe graph ids =
      case fringe of
        []   -> cont (apFst checkIfaceDates graph) ids
	id:ms ->
	      if isUnknown (fst graph) id
	      then visitModuleK (moduleName id ids) ids $ \ imports mod ids ->
		   traverseK (imports++ms)
                             (pwtg st (addModule id mod (fst graph)))
			     ids
	      else traverseK ms graph ids

    visitModuleK m ids cont =
        findModule m (srcdirs p) (libdirs p) errK srcK libK
      where
        errK = cont [] (nonexistentModule m) ids
        srcK path =
	  checkDatesK (objdir p) path $ \ lit dates ->
	  hIO (ReadFile (unpackPS (source' lit path))) $ \ (Str s) ->
	  let allimports = parseImports lit s
	      imports = filter (`notElem` ignoreimports p) allimports
	      (is,ids') = moduleIds imports ids
	  in --evalSpine is $ -- avoids holding s in memory too long
	     --seq (is==is) $
	     --trace "blaha" $
	     cont is (existingModule m path lit dates is) ids'
	libK path =
	  checkLibDatesK (objdir p) path $ \ dates ->
	  cont [] (libModule m path dates) ids

findModule' st@(S{project=p}) m = findModule m (srcdirs p) (libdirs p)

findModule hm srcdirs libdirs errK srcK libK =
    findK isSource msrcdirs (findK isIface mlibdirs errK libK) srcK
  where
    (mdir,m) = splitPath (modulePath hm)
    msrcdirs = mdirs srcdirs
    mlibdirs = mdirs libdirs
    mdirs = if mdir==nilPS
	    then id
	    else map (unpackPS.(`joinPath` mdir).packString)

    isSource _ filepath = packString filepath `elem` [source m,litSource m]
    isIface  d filepath = packString filepath==iface' (take 1 d=="/") m
      -- "ghc -hisuf" allows you to change the extension of interface files,
      -- but library interfaces (=imported via an absolute path) always
      -- keep the standard extension.

    findK isWanted ds errK okK =
      case ds of
        [] -> errK
	d:ds ->
	    hIOerr (ReadDirectory d) (const next) $ \(StrList files)->
	    if any (isWanted d) files
	    then okK (joinPath (packString d) m)
	    else next
	  where
	    next = findK isWanted ds errK okK

checkDatesK objdir path cont =
  dateK (litSource path) $ \ lsd ->
  dateK (source path) $ \ sd ->
  checkLibDatesK objdir path $ \ (FileDates _ id od) ->
  if lsd>sd
  then cont True  (FileDates lsd id od)
  else cont False (FileDates sd id od)

checkLibDatesK objdir path cont =
  dateK (iface path) $ \ id ->
  dateK (objectFile objdir path) $ \ od ->
  cont (FileDates Never id od)

updTargetDatesK tdates [] cont = cont tdates
updTargetDatesK tdates (t:ts) cont =
    updTargetDateK tdates t $ \ tdates' ->
    updTargetDatesK tdates' ts cont
  where
    updTargetDateK tdates t@(m,(n,typ)) cont =
      dateK (extend typ n) $ \ d -> -- file name? !!!
      cont (LM.add ((m,typ),d) tdates)

dateK filename cont =
    getModificationTime (unpackPS filename) errK okK
  where
    errK _ = cont Never -- !! what kind of error?
    okK = cont . At
{-
fileDate s = convTime (read (words s !! 10)) -- actually Time
-- hbc: the 10th word in a StatusFile response is the file modification time.
  where
    convTime = _mkClockTime . fromInt
    {-       
    -- stolen from FileStat.hs
    convTime t = addToClockTime (TimeDiff{tdYear = 0, tdMon = 0, tdDay = 0, tdHour = 0, tdMin = 0, tdSec = t, tdPicosec = 0}) clockTime1970
    clockTime1970 = toClockTime (CalendarTime{ctYear=1970, ctMon=January, ctDay=1, ctHour=0, ctMin=0, ctSec=0, ctPicosec=0, ctTZName="", ctTZ=0, ctIsDST=False})
   -}
-}
pwtg st@(S{project=p,linkedTargets=linkedTargets,ids=ids}) =   -- pair with todo graph
    pairwith $ begin rootTargets linkedTargets
  where rootTargets = map (apFst $ fst.flip moduleId ids) (targets p)

-- space leak prevention
--evalSpine [] = id
--evalSpine (x:xs) = evalSpine xs
