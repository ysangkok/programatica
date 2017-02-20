module ResourceIds where

newtype WindowId = WindowId Int deriving (Eq, Ord, Show)

type Window = WindowId
type XWId = WindowId

rootWindow = WindowId 0
noWindow = WindowId (-1)
windowNone = WindowId 0

newtype PixmapId = PixmapId Int deriving (Eq, Ord, Show, Read)
newtype DbeBackBufferId = DbeBackBufferId Int deriving (Eq, Ord, Show, Read)
newtype FontId = FontId Int deriving (Eq, Ord, Show)
newtype GCId = GCId Int deriving (Eq, Ord, Show)
newtype CursorId = CursorId Int deriving (Eq, Ord, Show)
newtype ColormapId = ColormapId Int deriving (Eq, Ord, Show)

defaultColormap = ColormapId 0
cursorNone = CursorId 0

newtype Atom = Atom Int deriving (Eq, Ord, Show)


type ColorName = String
type FontName = String

type Time = Int
currentTime = 0::Time

type Depth = Int

copyFromParent = 0 :: Depth
parentRelative = PixmapId 1
none = PixmapId 0

rootGC = GCId 0

