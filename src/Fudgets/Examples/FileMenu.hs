module Main where 

-- Example of application with standard file menu.
-- The menu can be extended.

import Fudgets

main = fudlogue $ shellF "Test" topF

-- Test fudget, with a stringF displaying the internal file commands
-- Replace the stringF with some appropriate fudget...!

topF = (startupF ["Command..."] stringF >=^< show,LeftOf) >#==< 
       filemenuF myothers myshow


data MyChoices = Choice_1 | Choice_2 deriving (Show,Eq)
myothers = [Choice_1,Choice_2]
myshow Choice_1 = "Choice 1"
myshow Choice_2 = "Choice 2"

---==================
--- 

-- Menu choices
data MenuChoices s = MenuNew | MenuOpen | MenuSave | MenuSaveAs |
                 MenuOther s deriving Eq
showMenu showother c = case c of
	 MenuNew -> "New"
	 MenuOpen -> "Open..."
	 MenuSave -> "Save"
	 MenuSaveAs -> "Save As..."
	 MenuOther c -> showother c

-- Commands as they are used in the application
type Name = String
data MenuCmd s = New | Save Name | Open Name | Other s
      deriving Show -- for testing!

--filemenuF :: Eq a => [a] -> (a -> String) -> F [Char] (MenuCmd a)
filemenuF othercmds showothers = fileF >==< 
     menuF "File" (map (\x->(x,showMenu showothers x)) 
        ([MenuNew,MenuOpen,MenuSave,MenuSaveAs]++map MenuOther othercmds))
	     
{-
file name control.
fileSP keeps track of an optional file name. To start with, and when
the user chooses "New", this becomes Nothing. This results in a file dialog
whenever the user chooses "Save". Otherwise, the old name is picked when 
"Save" is chosen.

fileSP controls a filePick dialog with the useful loopThroughRightF 
combinator.

-}

fileF = loopThroughRightF (absF (fileSP Nothing))
           (inputDialogF "File" oldFilePickF)
  where fileSP name = 
	  let same = fileSP name in
          getSP $ \msg ->
	  let openFileDia cmd prompt = putSP (Left (cmd,prompt)) same
	  in
	  case msg of
	    Right cmd -> case cmd of
		MenuNew -> putSP (Right New) $ fileSP Nothing
		MenuSave -> case name of 
		   Just f -> putSP (Right (Save f)) same
		   Nothing -> openFileDia Save "Save new file"
		MenuSaveAs -> openFileDia Save "Save file as"
		MenuOpen -> openFileDia Open "Open file"
		MenuOther c -> putSP (Right (Other c)) same 
	    Left (cmd,f) -> putSP (Right (cmd f)) $ fileSP (Just f)
------
inputDialogF :: String -> F a (InputMsg b) -> F (c,String) (c,b)
inputDialogF title f = filterJust >^^=< tagLeftF (snd >^=< 
	    inputPopupOptF title f Nothing >=^< (\x->(Just x,Nothing)))
  where
   filterJust = mapFilterSP (\msg -> case msg of	
		 (a,Just b) -> Just (a,b)
		 (a,Nothing) -> Nothing)

tagLeftF :: F a b -> F (c,a) (c,b)
tagLeftF f = combineSP >^^=< idLeftF f >=^^< splitSP

combineSP :: SP (Either a b) (a,b)
combineSP = getSP $ combineSP'
   where combineSP' x = 
           getSP $ \y -> 
	   case (x,y) of
	       (Left a,Right b) -> putsSP [(a,b)] $ getSP $ combineSP'
	       (Right b,Left a) -> putsSP [(a,b)] $ getSP $ combineSP'
	       _ -> combineSP' y

------
sep = id
