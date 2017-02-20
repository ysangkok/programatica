module Main where
import AllFudgets
--import GraphicsF
import Graphic
import Drawing
import FixedDrawing
import FlexibleDrawing
import Bitmaps
--import DrawingOps
import Gedit
import Gutils

main = fudlogue (shellF "Tst1" tstF)

tstF = gEditF (const id) initDrawing genMenu>==<cmdsF

cmdsF =
    spacer1F hCenterS $ tableF 5 $ untaggedListF
      [ b (const $ circleD) "Circle",
	b (const $ triangleD) "Triangle",
	--b (const $ fatTriangleD) "Fat Triangle",
	b (const node) "Tree Node",
	b (const $ hello) "Hello!",
	b (const $ mailbox) "Mail Box",
	b (dup horizontalP) "Dup H",
	b (dup verticalP) "Dup V",
	b fatG "Fat",
	b niceFontG "Nice Font",
	b frameG "Frame",
	(const.annot ("String",txtMenu).g)>^=<inputDoneSP>^^=<stringF>=^^<nullSP
      ]
  where
    dup p d = PlacedD p (ComposedD [d,d])
    b x lbl = const x >^=< buttonF lbl

initDrawing =
    --AttribD [GCLineWidth (Width 2)] $
    vbox
      [g hello,
       triangleD,
       circle2,
       stack [circleD,fatTriangleD]]


hello = fontG labelFont $ padG 3 $ g "Hello, world!"

mailbox = annot ("Mail Box",genMenu) $ 
          g (BitmapFile "/usr/local/X11/lib/bitmaps/boxdown.bm")

node = vbox [SpacedD hCenterS $ g circle,g (hFiller 2),
             hbox [g triangle, g triangle]]

circle2 = hbox [circleD,circleD]

triangle = FD (pP 140 100)
              [DrawLine (Line p1 p2) | (p1,p2) <- [(a,b),(b,c),(c,a)]]
   where a = pP 5 95
         b = pP 135 95
	 c = pP 70 5

triangleD = annot ("Triangle",gfxMenu) (g triangle)
fatTriangleD = fatG (g triangle)
circleD = annot ("Circle",gfxMenu) (g circle)
marginG = padG 10

frameG d = stack [padG 5 d,g frame]
niceFontG = fontG "-*-utopia-medium-r-*-*-100-*-*-*-*-*-iso8859-*"

circle = FD (pP 60 60) [DrawArc (rR 5 5 50 50) 0 (64*360)]

txtMenu = (niceFontG,"Nice Font"):genMenu
gfxMenu = (fatG,"Fat"):genMenu
genMenu = [(const circleD,"Circle")]
