module FiniteMap where
import qualified Data.Map as Map


type FiniteMap = Map.Map

emptyFM = Map.empty
lookupFM :: (Ord k, Ord (Map.Map k a)) => Map.Map k a -> k -> Maybe a
lookupFM = flip Map.lookup
addToFM m k v = Map.insert k v m
addToFM_C f m k v  = Map.insertWith f k v m
delFromFM m x = Map.delete x m
listToFM :: Ord k => [(k, a)] -> Map.Map k a
listToFM = Map.fromList
addListToFM :: (Foldable t0) => (Ord k, Ord v) => Ord (Map.Map k v) => Map.Map k v -> t0 (k, v) -> Map.Map k v
addListToFM = foldl (uncurry . addToFM)
addListToFM_C f = foldl (uncurry . addToFM_C f)
keysFM = Map.keys
eltsFM = Map.elems
fmToList = Map.toList
mapFM = Map.mapWithKey
lookupWithDefaultFM m v k = Map.findWithDefault v k m
