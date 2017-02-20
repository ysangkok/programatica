module GeditOptions where
import Fud(argFlag)

data GEditInputMode
  = Abbreviations
  | Completions
  | ParsedInput (String,String)
  deriving (Eq)

inputMode0 = if completions then Completions else Abbreviations

data GEditMenuMode
  = UntypedMenu
  | UntypedNoStatusMenu
  | TypecheckedMenu
  deriving (Eq)

type AutoScroll = Bool
type AutoNextGoal = Bool

menuMode0 = if untypedmenu then UntypedMenu else TypecheckedMenu

completions    = argFlag "completions"    False
untypedmenu    = argFlag "untypedmenu"    (not gfMode)
autoscroll0    = argFlag "autoscroll"     False
autonextgoal0  = argFlag "autonextgoal"   True

gfMode = argFlag "GF" False -- Used also in DrawOptions
