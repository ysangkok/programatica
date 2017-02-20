import ContribFudgets
import Fudgets

main = fudlogue $ shellF progName $ tstF

tstF = displayF >=^< show
        >==< tstMenuF
	>==< read >^=< stringInputF

alts0 = map dup ["P1","P2","P3"]
start0 = "P1"

rd = conv . read
  where conv (alts,start) = ([(alt,alt)|alt<-alts],start)

dup x = (x,x)

tstMenuF = menuBarF menuBar

menuBar =
   [item optionsMenu "Options"]

optionsMenu =
    menu (Transl id Just)
    [item menuLangAlts "Menu Language"
    ]
  where
    menuLangAlts = MenuDynRadioGroup tr menuLangs "Builtin"
      where
        tr = itemT
	itemT = Transl f g
	f (is,l) = ([itemValue i|i<-is],l)
	g (ls,l) = Just ([item l l|l<-ls],l)

    menuLangs = [item "Builtin" "Builtin"]
