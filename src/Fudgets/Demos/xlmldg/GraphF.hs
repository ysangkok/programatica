module GraphF(graphF) where
import AllFudgets
import Graph
import NodeF
import DrawGraph
import Scans(mapAccumL)
--import HO(apSnd)
import ListUtil(mapSnd)

exposeEdges gc _ = (Low (XCmd ClearWindow):) . map (exposeEdge gc)
exposeEdge gc (_,line) = Low (wDrawLine gc line)

edgeK es ers gc =
      getK $ \msg ->
	case msg of
	   Low (XEvt (Expose er n)) ->
		if n>0 then edgeK es (er:ers) gc
		else putsK (exposeEdges gc (er:ers) es) (edgeK es [] gc)
	   High (Left nrect) ->
		let (cmds,es') = mapAccumL (moveedge nrect gc) [] es
		in putsK cmds (edgeK es' ers gc)
	   _ -> edgeK es ers gc

moveedge (mvn,mvpos) gc cmds ((n @ (topn,botn)),(l @(Line top bot))) =
    let mv n p f = if n==mvn
		   then f mvpos
		   else p
    in let top' = mv topn top topSidePoint
           bot' = mv botn bot bottomSidePoint
    in let l' = Line top' bot'
    in let cmds' = if top'/=top || bot'/=bot
		   then Low (wDrawLine gc l):Low (wDrawLine gc l'):cmds
		   else cmds
    in (cmds',(n,l'))

--nodeFs = map (High . Left . apSnd (DynCreate . nodeF))
nodeFs = listLF overlayP . mapSnd nodeF

graphF unsizedgraph = 
  let wattrs =[CWEventMask [ExposureMask],
	       --CWBackingStore Always,
	       CWBitGravity NorthWestGravity]
      startcmds size = [XCmd $ ChangeWindowAttributes wattrs
		        --,layoutRequestCmd (plainLayout size True True)
			]
      gK nedges =
	  allocNamedColorPixel defaultColormap fgColor $ \fgc ->
	  changeGetBackPixel bgcolor $ \bgc ->
	  wCreateGC rootGC (GCLineWidth 2:invertColorGCattrs fgc bgc) $ \gc ->
	  putK (High (Right "Ready")) $
	  edgeK nedges [] gc
      route (n,Left y) = Left (n,y)
      route (n,Right x) = Right (n,x)
  in sizeGraph unsizedgraph $ \(size,Graph nnodes nedges) ->
     loopCompF
	 (groupF (startcmds size) (gK nedges)
	         (route>^=<nodeFs nnodes>=^^<nullSP))

sizeGraph graph fudget =
   safeLoadQueryFont menuFont $ \font->
   fudget (graph (rectsize . string_rect font))

bgcolor = argKey "bg" "skyblue"
