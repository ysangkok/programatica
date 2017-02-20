-- Diagram drawing example

import Fudgets

main = fudlogue (shellF "Diagram" mainF)

-- Show the two versions of the diagram
mainF = labelF diagram1 >+< labelF diagram2

-- diagram2 just adds color and line width to diagram1
diagram2 = fgD "blue" $ softAttribD [GCLineWidth 2] $ g diagram1

-- diagram1 is a simple line drawing of size (100,100)
diagram1 = FixD (Point 100 100) [DrawLines CoordModeOrigin points]

points = [Point 0 50, Point 20 80, Point 50 10,Point 70 30,
	  Point 90 100,Point 100 50]


{- See also the following manual pages:

  http://www.cs.chalmers.se/Fudgets/Manual/current/FixedDrawing.html
  http://www.cs.chalmers.se/Fudgets/Manual/current/DrawCommand.html
  http://www.cs.chalmers.se/Fudgets/Manual/current/Drawing.html

  http://www.cs.chalmers.se/Fudgets/Manual/current/labelF.html
  http://www.cs.chalmers.se/Fudgets/Manual/current/displayF.html

-}
