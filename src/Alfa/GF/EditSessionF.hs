module EditSessionF where

import Operations
import Fudgets
import Editing
import EditSession
import Tokens (Str(..))
import Grammar
import Lookup (allCats)
import State
import Option
import IOGrammar
import PrGrammar
import CustomCommands
import UnicodeF (writeInputF, writeOutputF)

fudlogueEdit :: FontId -> (EditEnv,[Option]) -> IO () -> IO ()
fudlogueEdit font (st,opts) resume =
  fudlogue $
   gfSizeP $
     shellF ("GF Fudget Editor for " ++ uwlangs) (gfF font opts langs st)
   where
      langs = map prLanguage (allLanguages (gStateOfEnv st))
      uwlangs = unwords langs

gfF font os ls st = 
  nameLayoutF gfLayout $ 
  (gfOutputF font st >==< gfCommandF font os ls st) >+< quitButF

(quitN : menusN : parseN : constraintN : newN : parsoptN : localwrapN :
  modifyN : viewN : undoN : randomN : outputN : saveN : _) = map show [1..]

gfLayout = placeNL verticalP [generics,output,menus,transform,parsing]
  where
    generics  = placeNL horizontalP (map leafNL [newN,saveN,viewN,parsoptN,quitN])
    output    = leafNL                           outputN
    menus     = leafNL                           menusN
    transform = placeNL horizontalP 
                  (map leafNL [randomN,modifyN,constraintN,{-localwrapN,-} undoN])
    parsing   = leafNL                           parseN
 
initFEState opts = addOptions opts emptyEState

gfSizeP      = spacerF (sizeS (Point 720 640))
controlSizeP = spacerF (sizeS (Point 720 360))
windowSizeP  = spacerF (sizeS (Point 600 300))
popupSizeP   = spacerF (sizeS (Point 240 100))

gfOutputF font st = 
--  shellF "GF Output" -- to create a separate window
  ((nameF outputN $ (writeFileF >+< textWindowF font)) 
     >==< 
   (absF (saveSP "EMPTY") 
                        >==< 
    (nameF saveN (popupStringInputF "Save" "foo.tmp" "Save to file:")
         >+<  
      mapF (prEditEnv st))))
   >==< 
  mapF Right 

gfCommandF :: FontId -> [Option] -> [String] -> EditEnv -> F () EState
gfCommandF font os ls gg = 
  loopCommandsF os gg >==< getCommandsF font ls gg >==< mapF (\_ -> Click)

loopCommandsF :: [Option] -> EditEnv -> F ECommand EState
loopCommandsF opts gg = loopThroughRightF (mapGfStateF opts gg) (mkMenusF gg)

mapGfStateF :: 
  [Option] -> EditEnv -> F (Either ECommand ECommand) (Either EState EState)
mapGfStateF opts gg = mapstateF execFC (initFEState opts) where
  execFC e0 (Left  c) = (e,[Right e,Left e]) where e = execECommand gg c e0
  execFC e0 (Right c) = (e,[Left e,Right e]) where e = execECommand gg c e0

mkMenusF :: EditEnv -> F EState ECommand
mkMenusF gg = 
 nameF menusN $
 placerF horizontalP $
 (labAboveF "Refine active goal" 
            (mapF fst >==< smallPickListF snd >==< mapF (mkRefineMenu False gg))
     >*<
 (labAboveF "Activate goal or delete subterm" 
            (changeF gg >==< smallPickListF snd >==< mapF (env2Change gg)))
     >*<
 (labAboveF "Wrap term in function" 
            (mapF fst >==< smallPickListF snd >==< mapF (env2FWrap False gg)))
 )

getCommandsF :: FontId -> [String] -> EditEnv -> F Click ECommand
getCommandsF font ls gg = 
    (refineParseF font gg >==< mapF (\_ -> ""))
          >*<
    (newF gg            >==< mapF (\_ -> ECVoid))
          >*<
    (termCommandF gg    >==< mapF (\_ -> ECVoid))
          >*<
       randomF
          >*<
       undoF
          >*<
      (optionF ls gg    >==< mapF (\_ -> ECVoid))
          >*<
      (parseOptionF ls gg    >==< mapF (\_ -> ECVoid))
          >*<
    (addConstrsF gg     >==< mapF (\_ -> ""))
----          >*<
----    (localWrapF gg      >==< mapF (\_ -> ""))


quitButF = 
  nameF quitN $
  quitF >==< buttonF "Quit"

refineParseF :: FontId -> EditEnv -> F String ECommand
refineParseF font gg =
  nameF parseN $
  mapF (ECParse []) >==< ("Refine by parsing" `labLeftOfF` writeInputF font)

addConstrsF :: EditEnv -> F String ECommand
addConstrsF gg =
  nameF constraintN $
  mapF (ECAddConstraints) >==< 
  popupStringInputF "Add constraint" "[??] = [??]" "Constraint:"

localWrapF :: EditEnv -> F String ECommand
localWrapF gg =
  nameF localwrapN $
  mapF parseLocalWrap >==< 
  popupStringInputF "Local Wrap" "0 f 1 2 3" "Wrap:"

newF :: EditEnv -> F ECommand ECommand
newF st = 
  nameF newN $
  menuF "New" [(ECNew c, prt c) | c <- allCats (abstractOfEnv st)]

termCommandF :: EditEnv -> F ECommand ECommand
termCommandF st = 
  nameF modifyN $
  menuF "Modify" [(ECTermCommand c,c) | (c,_) <- allTermCommands (abstractOfEnv st)]

optionF :: [String] -> EditEnv -> F ECommand ECommand
optionF langs st =
  nameF viewN $ 
  menuF "View" $ (ECOptions allLin,"All forms"):
                 [(ECOptions (iOpt s), s) | s <- "Abs":langs]

parseOptionF :: [String] -> EditEnv -> F ECommand ECommand
parseOptionF langs st =
  nameF parsoptN $ 
  menuF "Parser" $ [(ECOptions o,s) | (o,s) <-
                      [(chartParse,"chart"), (earleyParse,"Earley"),
                       (topdownParse,"top-down"),
		       (topdownPars2,"top-down-2"),(firstParse,"cutting"),
                       (forgiveParse,"correcting"),(literalParse,"literal"),
                       (ignoreParse,"ignoring")]]

undoF :: F Click ECommand 
undoF = 
  nameF undoN $ 
  mapF (\_ -> ECUndo) >==< buttonF "Undo"

changeF :: EditEnv -> F (Int -> ECommand, String) ECommand
changeF gg =  
  mapF (($ 0) . fst)
---  popupMenuF (zip [0..5] [0..5]) (mapF (($ 0) . fst)) >==< mapF Right

randomF :: F Click ECommand 
randomF =
  nameF randomN $ 
  mapF (\_ -> ECGenerateRandom) >==< buttonF "Random"

-- auxiliaries

showAndSaveF font fud = (writeFileF >+< textWindowF font) >==< saveF fud

saveF :: F a String -> F (Either String a) (Either (String,String) String)
saveF fud = 
  absF (saveSP "EMPTY") 
    >==< 
  (popupStringInputF "Save" "foo.tmp" "Save to file:" >+< fud)

saveSP :: String -> SP (Either String String) (Either (String,String) String)
saveSP contents = 
  getSP $ \msg -> case msg of
                    Left  file   -> putSP (Left (file,contents)) (saveSP contents)
                    Right string -> putSP (Right string)         (saveSP string)

textWindowF =   --- windowSizeP $ 
                writeOutputF

-- to replace stringInputF by a pop-up slot behind a button
popupStringInputF :: String -> String -> String -> F String String
popupStringInputF label deflt msg =
  mapF snd 
      >==<
  (popupSizeP $ stringPopupF deflt)
      >==<
  mapF (\_ -> (Just msg,Nothing)) 
      >==<
  decentButtonF label
      >==<
  mapF (\_ -> Click)

--            (labLeftOfF lab stringInputF >+< fud)

decentButtonF = spacerF (sizeS (Point 80 20)) . buttonF

-- this does not work...
popMenuF :: String -> [(ECommand,String)] -> F ECommand ECommand
popMenuF label alts = 
  mapF (either id (const ECVoid))
    >==<
  popupMenuF alts (buttonF label >==< constF Click)
    >==<
  constF (Left alts)

constF = mapF . const

