module ModuleListF(moduleListF) where
import Prelude hiding (FilePath)
import Fudgets hiding (Time)
import ContribFudgets(hSplitF')
import DependencyGraph(Graph,listGraph,lookupModule,references)
import FileNames
import Modules
import ModuleIds
import BufferButtonF
import DrawStatus
import Maybe(mapMaybe)
import PackedString(PackedString,unpackPS)
import Sort(sortLe)
import qualified IntSet as IS (IntSet,toList)

--import GFud

moduleListF =
    createDrawStatus $ \ drawStatus ->
    stripEither >^=<
    (serCompLeftToRightF $
     hSplitF' aLeft
              (throughF (modulePickF drawStatus))
              (idLeftF (moduleDispF drawStatus) >=^< route))
  where
    modulePickF drawStatus =
	placerF (permuteP [1,0,2] verticalP) $
	idLeftF (modPickListF >==< mapstateF ctrl state0) >==<
        route >^=< throughF ((noStretchF False True . hBoxF) (updAllButtonF >+< pickSortF)>=^<Left)
	>==< bypassF summaryF >=^< listGraph
      where
        summaryF = displayF' (setBorderWidth 0 . setMargin 2) >=^< summaryD

        summaryD ms = hboxD' 10 (mapMaybe countD [lib,good,old,err])
	  where
	    ss = map (modStatus.snd) ms
	    countD p =
	      case filter p ss of
		[] -> Nothing
		ps@(s:_) -> Just (hboxD' 0 [g (length ps),drawStatus s])
	    good = (==Good)
	    old = (==OutOfDate)
	    err = (`elem` [Unknown,Nonexistent,InError])
	    lib = (==Library)
		   
	updAllButtonF =
	  spacer1F (hAlignS aLeft) $
	  bufferButtonF' (key "a") "Update All" >=^< map (modName . snd)

	modPickListF =
            vScrollF (hyperGraphicsF' custom (g "%%%% Module List %%%%")) >=^<
	    Left . drawModuleList
          where custom = setSizing Dynamic . setAdjustSize False
        pickSortF =
	  spacer1F (hvAlignS aRight aCenter) $
  	  menuF "Sort By" [(sortStatusTime,"Status & Modification Time"),
			   (sortTime,"Modification Time"),
			   (sortStatusName,"Status & Name"),
	                   (sortName,"Name")]

        state0 = ([],sortStatusTime)
        ctrl (list,sort) = either changeSort newList
	  where
	    changeTo list' sort' = ((list',sort'),[sort' list'])
	    changeSort sort' = changeTo list sort'
	    newList list' = changeTo list' sort
        
	sortStatusTime = sortComp compStatusTime
	sortStatusName = sortComp compStatusName
	sortTime = sortComp compTime
	sortName = sortComp compName

	sortComp comp = sortLe le
	  where le (_,m1) (_,m2) = comp m1 <= comp m2

	compName = modName
	compStatusName m = (modStatus m,modName m)

	compTime = snd . compStatusTime
	compStatusTime m = (modStatus m,Rev $ sourceDate $ modDates m)
{-
	  where
	    compt Never = 0
	    compt (At t) = -t
-}

	drawModuleList = tableD' 1 2 . map drawMod
	  where
	    drawMod (id,Module {modName=name,modStatus=status,modPath=path}) =
	      labelD id $
	      boxD [drawStatus status,
	            spacedD (hMarginS 2 0) $ g (unpackPS name)]


    moduleDispF drawStatus =
      --shellF{-' (setVisible False)-} "Module Info" $
      placerF (noStretchS False False `spacerP` revP verticalP) $
      wCreateGCtx rootGCtx black  $ \ gcblack ->
      wCreateGCtx gcblack blue $ \ gcblue ->
      let 
        state0 = (error "ModuleListF.moduleDispF.state0 bug",Nothing)
        ctrl state@(graph,current) = either pick (either pick newGraph)
	  where
	    newGraph graph' = ((graph',current),
	                       maybe [] ((:[]).Left. drawModule graph') current)
	    pick id = ((graph,Just id),
	               [Left (drawModule graph id),
		        Right (modName (lookupModule graph id))])

	dispF = pickF

	drawModule graph id =
	  showmod drawStatus gcblack gcblue graph (id,lookupModule graph id)

	updButtonF = --teeF (modName.snd) "Update" >==<
	             (:[]) >^=< bufferButtonF' (key "u") "Update"
      in updButtonF >==<
	 loopThroughRightF (mapstateF ctrl state0) dispF

black = [GCFont listfont] :: [GCAttributes [String] String]
blue = [GCForeground ["blue3","black"]] ::[GCAttributes [String] String]

key k = setKeys [([],k)] -- keyboard shortcut, no modifiers

showStatus status =
  case status of
    Unknown     -> "-      "
    Nonexistent -> "missing"
    Good        -> "ok     "
    Library     -> "library"
    OutOfDate   -> "changed"
    InError     -> "*ERROR*"

showmod drawStatus gcblack gcblue graph
        (id,Module {modName=name,modStatus=status,modPath=path,
	            modDates=dates,modImports=imports}) =
  spacedD (marginHVAlignS 3 aLeft aTop) $
  vboxD' 0 $
  [g $ "module " ++ unpackPS name,
  hardAttribD gcblack $
  vboxD' 0 $
  [lineD [g "--status: ",drawStatus status, g $ ' ':showStatus status],
   g $ "--location: " ++ unpackPS path{-,
   paraD (g "--dates: ":(map g . words $ show dates))-}] ++
  listMods "Imports " (IS.toList imports) ++
  [spacedD (marginS 2) $ g $ hFiller 1] ++
  listMods "Imported by " usedby]
  where
    usedby = IS.toList (references graph id)
    link m = l m $ hardAttribD gcblue $ g $ mname m
    mname = unpackPS.modName.lookupModule graph
    l = labelD
    lineD = spacedD leftS . hboxD' 0
    paraD = placedD (paragraphP' (pP defaultSep 0)) . boxD

    listMods hdr ms =
      [lineD [g hdr,g (length ms),g " module(s):"],
       paraD (map link ms)]

pickF = vScrollF $ hyperGraphicsF (blankD 200) >=^< Left

-- route :: (a+b)+c -> a+(b+c)
route (Left (Left updall)) = Left updall
route (Left (Right sortby)) = Right (Left sortby)
route (Right newList) = Right (Right newList)

listfont = "-*-helvetica-medium-r-*-*-10-*-*-*-*-*-iso8859-1"

newtype Reverse a = Rev a deriving (Eq)

instance Ord a => Ord (Reverse a) where Rev x<=Rev y = y<=x
