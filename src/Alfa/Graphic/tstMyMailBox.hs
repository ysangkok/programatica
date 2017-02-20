import Fudgets
import Fud
import Graphic
import Drawing
import Bitmaps
import MeasuredGraphics(GCtx)
import MeasuredGraphics(MeasuredGraphics)
import Gutils
import GCAttrs
import HyperGraphics

main = fudlogue (shellF "Tst2" tstF)

tstF = hyperGraphicsF 1 bgColor myMailBox
  where
    myMailBox :: Drawing () Gfx
    myMailBox = vbox [mailbox, SpacedD centerS myname]

    myname = g "hallgren"

    mailbox = g (BitmapFile "/usr/local/X11/lib/bitmaps/boxdown.bm")
