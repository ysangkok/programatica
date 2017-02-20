module Main where
import AllFudgets --(oldScrollF)
import DrawGraph
import GraphF
import ShowFileF
import GraphUtils
import Graph(GRAPH)
import getdepend
import order
--import utils
import filenames hiding (isAbsolute)
import HO(apSnd)

main = fudlogue (showfileF>==<graphWindowsF)
-- fudlogue (map (trace "before grabF") . ((allcacheF . grabF) (showfileF:==:graphWindowsF)))

buildGraph =
    drawGraph .
    collapseCycles . order . optrevg .
    --collapse dir .
    graphmap executableFileName . map (apSnd tail) . snd . extractDependencies

graphWindowsF =
    if null args
    then dynGraphWindowF >==<
	 shellF "Select a module" ((:[])>^=<(stripInputSP>^^=<oldFilePickF))
    else showgraphF (buildGraph args)>==<nullF

dynGraphWindowF = snd>^=<dynListF>=^^<zipSP [1..]>=^<graphWindow

graphWindow = DynCreate . showgraphF . buildGraph

showgraphF unsizedg =
  shellF "Dependency graph"
    (oldScrollF True (pP 50 50,pP 600 600) (graphF unsizedg))

revopt = argKey "top" "up" == "up"
optrevg = if revopt then revg else id

--dir = fst . splitat '/'
