module TodoGraph(TodoGraph,Cmd(..),Target,begin,allDone,whatNow,doneWith{-,showTodo-}) where
import OrdGraph(OrdGraph)
import qualified OrdGraph as OG
import qualified OrdSet as OS (null,toList)
import qualified IntSet as IS (toList)
import Modules
import ModuleIds
import DependencyGraph
import FileNames(FileType(..),PFilePath,isLinkType)
import NonStdTrace(trace)
import PackedString(unpackPS)
import List(partition)
import Maybe(isJust,fromJust)
import Utils3

--- Interface -----------------------------------------------------------------

--data TodoGraph

type Target = (ModuleId,FileType)

data Cmd
   = Compile Bool PFilePath
   | Link FileType PFilePath [PFilePath]
   deriving (Show)

begin :: [Target] -> [(Target,FileDate)] -> Graph -> TodoGraph
--moreTodo :: Graph -> Target -> TodoGraph -> TodoGraph

whatNow :: Graph -> TodoGraph -> ([(Target,Cmd)],TodoGraph)

--doneWith :: Target -> TodoGraph -> TodoGraph
doneWith :: ModuleId -> TodoGraph -> TodoGraph


--- Implementation ------------------------------------------------------------

data TodoGraph
  = T {doing::[Target],
       linked::[((Target,[Target]),FileDate)],
       graph,revGraph:: OrdGraph Target}
  deriving (Show)

begin roots ldates graph = T [] lroots g (OG.reverse g)
  where
    g = foldr addRoot objectg lroots
      where addRoot = OG.addNode . fst
    lroots = map (apFst (pairwith linkObjs)) . filter (isLinkType.snd.fst) $ ldates
    linkObjs (m,_) = (m,Object):modImportList (lookupModule tgraph m)
    tgraph = transitiveClosure graph

    objectg = foldr addNode OG.empty (listGraph graph)
      where addNode (m,mod) = OG.addNode ((m,Object),modImportList mod)

allDone todo = null (doing todo)

whatNow g todo =
    (donow++do'',todo'')
  where
    (do',todo') = whatNow' todo
    (donow,done) = mapPair (map (apSnd fromJust),map fst) $
		   partition (isJust.snd) (map (pairwith docmd) do')
    (do'',todo'') = if null done
		    then ([],todo')
		    else whatNow g (foldr (doneWith.fst) todo' done)
    docmd target@(id,typ) =
        if isLinkType typ
        then case [r | r@((t,_),_)<-linked todo,t==target] of
	      ((t,objs),d):_ ->
		if any ((>d) . oDate . lookupModule g . fst) objs
		then Just (linkCmd objs)
		else Nothing
        else if (modStatus.lookupModule g) id==OutOfDate
	     then Just compCmd
	     else Nothing
      where
	mod = lookupModule g id
	compCmd = Compile (modIsLiterate mod) (modPath mod)
	linkCmd objs =
	  Link typ (modPath mod)
		   [modPath (lookupModule g o) |
		      (o,_)<-objs,
		      modStatus (lookupModule g o)/=Library
		      ]

whatNow' todo =
    (do', todo { doing = do'++doing todo, graph=graph' })
  where
    (now,later) = partition (OS.null.snd) (OG.toList (graph todo))
    do' = map fst now
    graph' = OG.fromList later

doneWith m todo =
  --trace (unwords ["doneWith",show m,show todo]) $
  case partition ((==m).fst) (doing todo) of
    ([t],doing') ->
      todo { doing=doing', 
	     graph=remEdges t (graph todo) es,
	     revGraph={-remEdges t-} (OG.remNode t (revGraph todo)) {-res-}
		  -- don't need remEdges t: no edges to t can remain in revGraph
	   }
      where
	es = neighbourList (revGraph todo)
	res = neighbourList (graph todo)
	remEdges n = foldr (flip OG.remEdge n)
	neighbourList g = OS.toList (OG.neighbours g t)
    ([],_) -> -- This can happen if the todo graph is reconstructed while
	      -- a compilation is in progress.
	      -- error "TodoGraph.doneWith []"
    	      todo
    _ -> error (unwords ["TodoGraph.doneWith",show m,show todo])

modImportList imports = [(i,Object) | i<-IS.toList (modImports imports)]


--
{-
showTodo ids T {doing,graph,revGraph} =
    unlines ("--T--":unwords (map showT doing): showG graph ++ showG revGraph)
  where
    showT (i,t) = unpackPS (extend t (moduleName i ids))
    showG = ([]:) . map showM . OG.toList
    showM (t,ts) = showT t ++ ": " ++ unwords (map showT (OS.toList ts))
-}
