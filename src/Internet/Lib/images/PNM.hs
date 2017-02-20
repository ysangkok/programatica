module PNM where
import Fudgets(Size(..),RGB)

data PNM = PNM Size PNMdata --deriving (Show)

data PNMdata = PPM Int [RGB] -- maximal color component value, rgb values
             | PGM Int [Gray] -- maximal gray value, gray values
	     | PBM [Bit]
	     --deriving (Show)

type Gray = Int
type Bit = Bool -- where False==White, True=Black
