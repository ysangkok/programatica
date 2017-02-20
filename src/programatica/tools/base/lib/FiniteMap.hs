module FiniteMap where
import qualified Data.Map as Map


type FiniteMap = Map.Map

emptyFM = Map.empty
lookupFM = flip Map.lookup
addToFM m k v = Map.insert k v m
addToFM_C f m k v  = Map.insertWith f k v m
delFromFM m x = Map.delete x m
listToFM = Map.fromList
addListToFM = foldl (uncurry . addToFM) 
addListToFM_C f = foldl (uncurry . addToFM_C f)
keysFM = Map.keys
eltsFM = Map.elems
fmToList = Map.toList
mapFM = Map.mapWithKey
lookupWithDefaultFM m v k = Map.findWithDefault v k m
