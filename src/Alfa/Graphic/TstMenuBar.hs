import Fudgets hiding (menuF,EditCmd,menuPopupF)
import ContribFudgets

#define CON(c) (Transl (c) (\x->case x of c y->Just y;_->Nothing))

dsubMenuItem = delayedSubMenuItem

main = fudlogue $ shellF "TstMenuBar" tstMenuBarF

tstMenuBarF = displayF>=^<show >==< menuBarF tstMenuBar

tstMenuBar = stdMenuBar ++ tstMenu

tstMenu = [dsubMenuItem CON(App) appMenu "App"]

data App
  = App1 Int
  | App2 Int
  | App3 Bool
  | App4 Ordering
  deriving (Show,Eq)

appMenu =
  [dsubMenuItem CON(App1) (subMenu 10) "App1",
   dsubMenuItem CON(App2) (subMenu 20) "App2",
   toggleItem CON(App3) False "App3",
   sepItem,
   radioGroupItem CON(App4) app4menu GT "App4"]

app4menu = [item GT "Greater",item EQ "Equal",item LT "Less"]

subMenu n =
  [cmdItem i ("Sub"++show i) | i<-[n+1..n+2]]

stdMenuBar =
  [dsubMenuItem CON(File) fileMenu "File",
   dsubMenuItem CON(Edit) editMenu "Edit"]

fileMenu =
  [txtCmdItem New,
   cmdItem    Open   "Open...",
   txtCmdItem Save,
   cmdItem    SaveAs "Save As...",
   cmdItem    Close  "Close...",
   txtCmdItem Quit]

txtCmdItem s = cmdItem s (show s)

editMenu =
  [txtCmdItem Undo,
   txtCmdItem Redo,
   sepItem,
   txtCmdItem Cut,
   txtCmdItem Copy,
   txtCmdItem Paste]

data StdMenuCmd appcmd
  = File FileCmd | Edit EditCmd | App appcmd 
  deriving (Eq,Show)

data FileCmd
  = New | Open | Save | SaveAs | Close | Quit
  deriving (Eq,Show)

data EditCmd
  = Undo | Redo | Cut | Copy | Paste
  deriving (Eq,Show)
