module DependencyGraph {-(module DependencyGraph,IntMap,IntSet)-} where
import Prelude hiding (FilePath)
import FileNames
import ModuleIds
import Modules
import qualified IntMap as IM
import qualified IntSet as IS
--import qualified PackedString as PS (nil)
--import ListUtil(nubEq)
--import Trace(trace)

#ifdef __HASKELL98__
#define map fmap
#endif

--im_map = IM.amap
im_map = map

newtype Graph = Graph (IM.IntMap Module)
unGraph (Graph g) = g

emptyGraph = Graph IM.empty
listGraph = IM.toList .unGraph

addModule id mod = Graph. IM.add (id,mod).unGraph
removeModule id = Graph. IM.fromList . filter ((/=id).fst). IM.toList .unGraph
--removeModule id = addModule id (nonexistentModule "?") -- !!

updateModule f id g = addModule id (f (lookupModule g id)) g

inError = updateModule (`setStatus` InError)

allOk = all (isOk . modStatus . snd) . IM.toList . unGraph

safeLookupModule (Graph g) id = IM.lookup id g
lookupModule g id = case safeLookupModule g id of Just m -> m

references g id =
  IS.fromList [i | (i,mod)<-listGraph g, id `mElem` modImports mod]

isUnknown graph id =
  case safeLookupModule graph id of
    Nothing -> True
    Just mod -> modStatus mod == Unknown

modImportMods graph = map (lookupModule graph). IS.toList .modImports

{-
transitiveImports g mod = nubEq sameMod (timps [mod] [])
  where
    sameMod m1 m2 = modName m1 == modName m2
    timps [] _ = []
    timps (mod:mods) visited =
        if name `elem` visited
	then []
	else imps ++ timps (imps++mods) (name:visited)
      where
        name = modName mod
	imps = map (lookupModule g) (modImports mod)
-}

transitiveClosure (Graph mods) = {-trace "transitiveClosure"-} tgraph
  where
    tgraph = Graph tmods
    tmods = im_map transitiveImports mods
    transitiveImports (Module {modName=m,modPath=path,modImports=imports}) =
      Module m e path e e
        (foldr1 IS.union
	   (imports:
	    map (modImports.lookupModule tgraph) (IS.toList imports)))
    e = error "DependencyGraph.transitiveClosure"
  
checkIfaceDates g@(Graph mods) = Graph (im_map chk mods)
  where
    chk mod = if any ((>odate).iDate) (IS.toList (modImports mod))
              then setOutOfDate mod
	      else mod
      where
        odate = oDate mod

    iDate name = case safeLookupModule g name of
                   Just mod -> ifaceDate (modDates mod)
		   _ -> Never

updModDate id lit dates g@(Graph mods) = Graph (im_map upd mods)
  where
    name = modName (lookupModule g id) -- hmm
    upd mod = if modName mod == name
              --then mod `setDates` dates `setStatus` Good   -- hmm
              then mod{modDates=dates,modStatus=Good,modIsLiterate=lit}
	      else if id `mElem` modImports mod && oDate mod<ifaceDate dates
	           then setStatus mod OutOfDate
		   else mod
