module Visual where
import HbcWord

data DisplayClass
  = StaticGray | GrayScale
  | StaticColor | PseudoColor
  | TrueColor | DirectColor
  deriving (Eq,Show,Enum)

newtype VisualID = VisualID Int deriving (Eq,Show)

data Visual
  = Visual { visualid :: VisualID,
	     visualClass :: DisplayClass,
	     red_mask,green_mask,blue_mask :: Word,
	     bits_per_rgb :: Int,
	     map_entries :: Int
	   }
  deriving (Show)
