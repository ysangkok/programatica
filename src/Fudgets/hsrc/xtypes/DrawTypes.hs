{-# LANGUAGE CPP #-}
module DrawTypes where
import Geometry(Point,Rect,Line)
import PackedString(ByteString)
import Xtypes(Pixel,PixmapId,ImageFormat,DbeBackBufferId)

#ifdef __GLASGOW_HASKELL__
import Data.Ix
-- fromEnum bug workaround
#define IX , Ix
#else
#define IX
#endif

data DrawCommand
-- Don't forget to change ../types/Drawcmd.hs too, if you change things here!!
  = DrawLine Line
  | DrawImageString Point String
  | DrawString Point String
  | DrawRectangle Rect
  | FillRectangle Rect
  | FillPolygon Shape CoordMode [Point]
  | DrawArc Rect Int Int
  | FillArc Rect Int Int
  | CopyArea Drawable Rect Point
  | CopyPlane Drawable Rect Point Int
  | DrawPoint Point
  | CreatePutImage Rect ImageFormat [Pixel]
  --
  | DrawImageStringPS Point ByteString
  | DrawStringPS Point ByteString
  --
  | DrawLines CoordMode [Point]
  | DrawImageString16 Point String
  | DrawString16 Point String
  deriving (Eq, Ord, Show)

data Drawable
  = MyWindow
  | Pixmap PixmapId
  | DbeBackBuffer DbeBackBufferId
   deriving (Eq, Ord, Show)

data CoordMode
  = CoordModeOrigin
  | CoordModePrevious 
  deriving (Eq, Ord, Show, Enum IX)

data Shape
  = Complex
  | Nonconvex
  | Convex
  deriving (Eq, Ord, Show, Enum IX)
