module SimpleF(Drawer(..), Fms'(..), MapState(..), simpleF, simpleK) where
import Color
import Command
import XDraw
import Dlayout(windowF)
import DShellF
--import FDefaults
--import Event
import Fudget
import FudgetIO
import FRequest
--import Xcommand
import Gc
import Geometry(Size)
import LayoutRequest
--import Message(Message(..))
import NullF
--import Spops
import MapstateK
import Xtypes

type MapState a b c = a -> b -> (a, c)

type Fms' a b c = MapState a (KEvent b) [KCommand c]

type Drawer = DrawCommand -> FRequest

simpleK :: (Drawer -> Drawer -> Fms' a b c) -> Size -> a -> K b c
simpleK k size s0 =
    allocNamedColorPixel defaultColormap "black" $ \fg ->
    allocNamedColorPixel defaultColormap "white" $ \bg ->
    wCreateGC rootGC [GCFunction GXcopy,GCForeground fg,GCBackground bg] $ \fgc ->
    wCreateGC fgc [GCForeground bg] $ \bgc ->
    putLow (layoutRequestCmd (plainLayout size True True)) $
    mapstateK (k (wDraw fgc) (wDraw bgc)) s0

simpleF title k size s0 =
    shellF title $
    windowF [XCmd $ ChangeWindowAttributes [CWEventMask eventmask]] $
    simpleK k size s0
  where
    eventmask = [ExposureMask, KeyPressMask, KeyReleaseMask, ButtonPressMask,
                 ButtonReleaseMask]
