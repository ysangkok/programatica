module Image where

newtype ImageFormat = ImageFormat Int deriving (Eq, Ord, Show)

xyBitmap = ImageFormat 0
xyPixmap = ImageFormat 1
zPixmap  = ImageFormat 2
