module Main where
import Fudgets
import DrawGraph
import GraphF
import ShowFileF
import GraphUtils
import Graph(GRAPH)
import getdepend
import order
--import utils
import filenames

main = fudlogue (allcacheF (showfileF>==<graphWindowsF))
-- fudlogue (map (trace "before grabF") . ((allcacheF . grabF) (showfileF>==<graphWindowsF)))

buildGraph =
    drawGraph .
    collapseCycles . order . optrevg .
    --collapse dir .
    graphmap executableFileName . map (asnd tail) . snd . extractDependencies

graphWindowsF =
    if null args
    then dynGraphWindowF >==<
	 simpleShellF "Select a module" [] Nothing ((:[])>^=<(stripInputMsg>^=<oldFilePickF))
    else showgraphF (buildGraph args)>==<nullF

dynGraphWindowF = snd>^=<dynListF>=^^<zipSP [1..]>=^<graphWindow

graphWindow = DynCreate . showgraphF . buildGraph

showgraphF unsizedg =
  shellF "Dependency graph"
    (stripEither>^=<
     ((oldScrollF (pP 50 50,pP 600 600)
                  (graphF unsizedg),Above)>#+<quitButtonF))

revopt = argKey "top" "up" == "up"
optrevg = if revopt then revg else id

--dir = fst . splitat '/'
