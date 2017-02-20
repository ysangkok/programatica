module Gutils where
import AllFudgets
import Drawing
import Graphic
--import MeasuredGraphics(MeasuredGraphics)
import FixedDrawing
import GCAttrs
import MakeGC
import Placers2

data Gfx = (Graphic ?a) => G ?a

--instance Text G where showsPrec _ _ s = "G "++s

instance Graphic Gfx where
  measureGraphicK (G x) = measureGraphicK x

boxVisibleD = ComposedD
boxD ds = ComposedD (length ds) ds
stackD = PlacedD overlayP . boxD
vertD = PlacedD verticalP
vertD' = PlacedD . verticalP'
horizD = PlacedD horizontalP
horizD' = PlacedD . horizontalP'
vboxD = vertD . boxD
hboxD = horizD . boxD
vboxD' sep = vertD' sep . boxD
hboxD' sep = horizD' sep . boxD

vertlD = PlacedD verticalLeftP
vertlD' = PlacedD . verticalLeftP'
vboxlD = vertlD . boxD
vboxlD' sep = vertlD' sep . boxD
horizcD = PlacedD horizontalCenterP
horizcD' = PlacedD . horizontalCenterP'
hboxcD = horizcD . boxD
hboxcD' sep = horizcD' sep . boxD

tableD n = PlacedD (tableP n) . boxD
tableD' sep n = PlacedD (tableP' n Horizontal sep) . boxD
g = AtomicD. G

westD = spacedD $ hvAlignS aLeft aCenter
northwestD = spacedD $ hvAlignS aLeft aTop


padD = spacedD.marginS

fontD fn = softAttribD [GCFont (FontSpec fn)]
fgD color = softAttribD [GCForeground (ColorSpec color)]
fatD = softAttribD [GCLineWidth (Width 5),GCCapStyle CapRound]
fgnD = fgD.Name
fontnD = fontD.Name

attribD = belowAnnot.AttribD
softAttribD = attribD.SoftGC
hardAttribD = attribD.HardGC
spacedD = belowAnnot.SpacedD

belowAnnot f (LabelD a d) = LabelD a (belowAnnot f d)
belowAnnot f d = f d

holeD =
    fgD (Name "blue3") $ rectD size
    --stack [fgD "white" (filledRectD size),rectD size]
  where
    size = pP 15 13

filledRectD size = g (FD (size) [FillRectangle (Rect origin size)])
rectD size = g (FD (size) [DrawRectangle (Rect origin size)])
