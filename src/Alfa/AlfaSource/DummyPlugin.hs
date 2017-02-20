module DummyPlugin where
import AlfaPluginKit
import FileConv(prSyntax)
import Char(isSpace)
import qualified ProofEngine as PE
import Fud
import Fudgets
--import qualified UDescribe as UD
--import Debug2(badtrace)

default(Int)

myname = "Dummy"

plugin =
  Methods {
    name = myname,
    state0 = \ _ -> return (0::Int),
    loadDecls =
      \ dsrc s _ ->
      if isImported dsrc -- hmm
      then const (Right s)
      else Right . (+1) . readCnt . fst . head . lex . concat . map snd,
    save = \ st -> [show st ++ " Dummy plugin was here!"],
    describe = descr,
    altdisp = \ st -> [("Plain",showAsPlain)],
    altparse = \ st -> [],
    fileparsers = [("txt",txtparser)]
  }
  where
    readCnt s = case reads s of
		  (n,_):_ -> n
		  _ -> 0

    txtparser st imported filename contents = Right (Module [cmnt],st)
      where
        cmnt = Comment ("{-\n"++contents++"\n-}")

    descr st menv (sy:ancestors) =
      --trace (unlines . map prSyntax $ sy:ancestors) $
      case sy of
        ModuleS (Module ds) ->
	    (Just (myname++" plugin is awake! "++show st),menu)
	  where
	    menu = [("Edit counter",editCounterCmd st),
		    ("Save counter in a separate file",saveCounter st),
		    ("Graphics test",gfxTest st),
		    ("Crash Alfa",error "Dummy plugin deliberately crashed Alfa"),
		    checkTermMenu ds]
	ExpS e ->
          case e of
	    EAnnot (AsTextBy _) _ -> (Nothing,[])
	    _ -> (Nothing,[("Show as plain text",showAsPlainCmd e),
			   ("Show in the graphics window",displaySyntax sy)])
	DeclS d -> (Nothing,
		    [("Insert counter value below",
		      Replace (DeclsS [d,Comment ("-- "++show st)])),
		     checkTermMenu [d]])
	DeclsS ds -> (Nothing,[checkTermMenu ds])
        _ -> (Nothing,[])


    saveCounter st = SaveFileAs (Just "dummycounter.txt") (show st)

    gfxTest st = DisplayGfx gfx
      where
        gfx = G $ fancyTextD $ "The current value of the counter is "++show st

    showAsPlainCmd = Replace . syn . EAnnot (asTextBy (myname,"Plain"))

    showAsPlain menv = map showPara . lines . prSyntax
      where
        showPara s =
	  case span isSpace s of
	    (sp,txt) ->
		--(length sp,map TPlain $ words txt)
	        PlainPara (map TPlain $ words txt)

    editCounterCmd st =
        EditWith editargs -- $ \ st' ->
	--EditWithText "You have a chance to do it again" (show st') parse
	ReplaceState
      where
        editargs = (stringEdit "Edit save count" parse){def=Just $ show st}
        --parse :: TextInputParser Int
        parse s =
	  case reads s of
	    (n,r):_ ->
	      case lex r of
	        ("",""):_ -> Right n
		_ -> Left ((0,length s-length r),"trailing garbage")
	    _ -> Left ((0,0),"syntax error")

    checkTermMenu ds = (myname++": Check Termination",checkTerm ds)
    checkTerm ds = 
      GetPEState $ \ peState ->
      Message $
      case PE.query peState (PE.checkTermination ds) of
        Left err -> Left err
	Right () -> --badtrace "termination ok" $
                    Right "Agda's Termination Check succeeded"

-- From FanceHello.hs:
fancyTextD text =
    stackD [bgD,
	    spacedD (hvMarginS (5+dist) 5) shadowTxtD,
	    spacedD (hvMarginS 5 (5+dist)) fgTxtD]
  where
    shadowTxtD = fgD shadow txtD
    fgTxtD = fgD color txtD
    txtD = spacedD centerS $ fontD font (g text)
    bgD = vboxD' 0 [fgD sky fillD,fgD ground fillD]
    fillD = g (filler False False 10)

-- Everything can be changed with command line switches:
color = argKey "color" "red"
shadow = argKey "shadow" "black"
sky = argKey "sky" "skyblue"
ground = argKey "ground" "blue"
greeting = argKey "greeting" "Hello, world!"
dist = diag (argReadKey "dist" 2)
font = argKey "font" "-*-helvetica-bold-r-*-*-24-*-*-*-*-*-iso8859-1"
