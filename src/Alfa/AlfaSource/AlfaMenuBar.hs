module AlfaMenuBar where
import Fudgets(noStretchF)
import Fud(Gfx(..),blankD)
--import MenuBarF
import ContribFudgets
--import FileMenuF(FileMenuItem(..))
--import KeyMenuF
import DrawOptions(ProofStyle(..),proofStyle0,DeclDetail(..),autoAbstract0,onlyRefine0,declDetail0,LayoutDirection(..))
import GeditOptions
import Fonts(fontsize)
import ListUtil(mapSnd)

-- If Haskell only had first class constructors...
#define CON(c) (Transl (c) (\x->case x of c y->Just y;_->Nothing))

data FileMenuCmd
  = New
  | Open
  | Import
  | Browse
  | Save
  | SaveAs
  | PrintToFile
  | Quit
  deriving (Eq,Show)

data EditMenuCmd
  = Undo
  | Redo
  | Copy'
  | Paste'
  | AppendDecl
  | Delete
  | Give
  | Refine
  deriving (Eq)

data ViewCmd
  = Hide Bool
  -- | HideEdit
  | Compact Bool
  | LayoutStyle LayoutDirection
  | DeclDetail DeclDetail
  | PStyle ProofStyle
  | HideTrivialLet Bool
  | UnfoldGoals Bool
  | FontSize Int
  | Redraw
  | ZoomIn
  | ZoomOut
  deriving (Eq)

data OptionsCmd
  = InputMode GEditInputMode
  | MenuMode GEditMenuMode
  | MenuLang MenuLangs
  | AutoAbstract Bool
  | OnlyRefine Bool
  | SimpleRefine Bool
  | AutoSolve Bool
  | AutoScroll Bool
  | AutoNextGoal Bool
  deriving (Eq)

type MenuLangs = ([MenuLang],MenuLang)
data MenuLang = BuiltinLang | PluginLang (String,String) deriving (Eq)
menuLang0 = BuiltinLang

data UtilsCmd
  = EvalWindow Bool
  | MenuWindow Bool
  deriving (Eq)

data MenuCmd filecmd
  = File filecmd
  | Edit EditMenuCmd
  | View ViewCmd
  | Options OptionsCmd
  | Utils UtilsCmd
  deriving (Eq)

--alfaMenuBarF = alfaMenuBarF' [] []

alfaMenuBarF' ed em = noStretchF False True (menuBarF (menuBar ed em))

menuBar extraDeclDetails extraInputModes =
  [item fileMenu  "File",
   item editMenu  "Edit",
   item (viewMenu extraDeclDetails)  "View",
   item (optionsMenu extraInputModes) "Options",
   item utilsMenu "Utils"]

fileMenu =
  menu CON(File)
  [cmdItem New		"New"		`key` "n",
   cmdItem Open		"Open..."	`key` "o",
   cmdItem Import	"Import..."	`key` "i",
   cmdItem Browse	"Browse...",
   cmdItem Save		"Save"		`key` "s",
   cmdItem SaveAs	"Save As..."	`key` "a",
   cmdItem PrintToFile	"Print To File..." `key` "p",
   cmdItem Quit		"Quit"		`key` "q"]

editMenu =
  menu CON(Edit)
  [cmdItem Undo		"Undo"		`key` "z",
   cmdItem Redo		"Redo"		`key` "period",
   sepItem,
   cmdItem Copy'	"Copy"		`key` "c",
   cmdItem Paste'	"Paste"		`key` "v",
   cmdItem Delete	"Delete"	`key` "x",
   sepItem,
   cmdItem AppendDecl	"New Declaration..."	`key` "d",
   cmdItem Give		"Give..."	`key` "g",
   cmdItem Refine	"Refine..."	`key` "r"]

viewMenu extraDeclDetails =
    menu CON(View)
    [cmdItem Redraw		  "Redraw"		`key` "comma",
     cmdItem ZoomIn               "Zoom in"             `key` "less",
     cmdItem ZoomOut              "Zoom out"            `key` "greater",
     sepItem,
     toggleItem CON(UnfoldGoals) False "Unfold Goal Types" `key` "u",
     sepItem,
     toggleItem CON(Hide)    True "Argument Hiding"	`key` "h",
     toggleItem CON(Compact) True "Brief Notation"	`key` "b",
     toggleItem CON(HideTrivialLet) False "Hide Type Annotations" `key` "slash",
     sepItem,
     item layoutAlts		  "Default Layout",
     sepItem,
     item declDetailAlts          "Default Declaration View",
     sepItem,
     item proofAlts		  "Proof Style",
     sepItem,
     item fontSizeAlts		  "Font Size"]
  where
    layoutAlts = MenuRadioGroup CON(LayoutStyle) layoutStyles Wide
    layoutStyles = [item Wide "Wide" `key` "w",
		    item Tall "Tall"   `key` "t"]

    proofAlts =  MenuRadioGroup CON(PStyle) proofStyles proofStyle0
    proofStyles =
	[item UglyProof      "Plain"			`key` "minus",
	 item NDProof        "Natural Deduction"	`key` "plus",
	 item BrorProof      "Top-Down Proof Tree"	`key` "apostrophe"]

    declDetailAlts = MenuRadioGroup CON(DeclDetail) declDetails declDetail0
    declDetails =
        [item JustNames "Show defined names only",
	 item NamesAndTypes "Show type signatures only",
	 item CompleteDecls "Show complete definitions"]
        ++ [item d s | (d,s)<-extraDeclDetails]

    fontSizeAlts = MenuRadioGroup CON(FontSize) fontSizes fontsize
    fontSizes = [item n n `key` show n|n<-[1..7]]

optionsMenu extraInputModes =
    menu CON(Options)
    [item inputModeAlts "Input Mode",
     sepItem,
     item menuModeAlts  "Menu Mode",
     blankItem,
     toggleItem CON(OnlyRefine) onlyRefine0 "Only refinements from context",
     sepItem,
     item menuLangAlts "Menu Language",
     sepItem,
     toggleItem CON(AutoAbstract) autoAbstract0 "Auto Abstract",
     toggleItem CON(AutoSolve) True "Auto Solve Constraints",
     toggleItem CON(SimpleRefine) False "Simple Refine",
     toggleItem CON(AutoScroll) autoscroll0 "Auto Scroll to Center Cursor",
     toggleItem CON(AutoNextGoal) autonextgoal0 "Auto Goto Next Goal"
    ]
  where
    inputModeAlts = MenuRadioGroup CON(InputMode) inputModes inputMode0

    inputModes =
        [item Abbreviations "Abbreviations",
	 item Completions   "Completions"] ++
        [item m s | (m,s)<-extraInputModes]

    menuModeAlts = MenuRadioGroup CON(MenuMode) menuModes menuMode0

    menuModes =
        [item UntypedMenu "Show everything in scope",
	 item UntypedNoStatusMenu "Show everything, no status",
	 item TypecheckedMenu "Filter through type checker"]

    menuLangAlts = MenuDynRadioGroup tr menuLangs menuLang0
      where
        tr = CON(MenuLang) `compT` itemT
	itemT = Transl f g
	f (is,l) = ([itemValue i|i<-is],l)
	g (ls,l) = Just ([item l (shLang l)|l<-ls],l)

    menuLangs = [item menuLang0 (shLang menuLang0)]

    shLang BuiltinLang = "Builtin"
    shLang (PluginLang (pl,ln)) = pl++": "++ln

utilsMenu =
  menu CON(Utils)
  [toggleItem CON(EvalWindow) False "Evaluate Expressions" `key` "e",
   toggleItem CON(MenuWindow) True  "Menu Window"          `key` "m"]

blankItem = item MenuLabel (blankD 5)
