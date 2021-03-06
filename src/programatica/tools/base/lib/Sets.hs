module Sets(module Sets,Set,union) where

import Data.Set

{-# DEPRECATED emptySet "Use empty instead" #-}
-- | Obsolete equivalent of 'empty'.
emptySet :: Set a
emptySet = empty

{-# DEPRECATED mkSet "Use fromList instead" #-}
-- | Obsolete equivalent of 'fromList'.
mkSet :: Ord a => [a]  -> Set a
mkSet = fromList

{-# DEPRECATED setToList "Use elems instead." #-}
-- | Obsolete equivalent of 'elems'.
setToList :: Set a -> [a] 
setToList = elems

{-# DEPRECATED unitSet "Use singleton instead." #-}
-- | Obsolete equivalent of 'singleton'.

unitSet :: a -> Set a
unitSet = singleton

{-# DEPRECATED elementOf "Use member instead." #-}
-- | Obsolete equivalent of 'member'.
elementOf :: Ord a => a -> Set a -> Bool
elementOf = member

{-# DEPRECATED isEmptySet "Use null instead." #-}
-- | Obsolete equivalent of 'null'.
isEmptySet :: Set a -> Bool
isEmptySet = Data.Set.null

{-# DEPRECATED cardinality "Use size instead." #-}
-- | Obsolete equivalent of 'size'.
cardinality :: Set a -> Int
cardinality = size

{-# DEPRECATED unionManySets "Use unions instead." #-}
-- | Obsolete equivalent of 'unions'.
unionManySets :: Ord a => [Set a] -> Set a
unionManySets = unions

{-# DEPRECATED minusSet "Use difference instead." #-}
-- | Obsolete equivalent of 'difference'.
minusSet :: Ord a => Set a -> Set a -> Set a
minusSet = difference

{-# DEPRECATED mapSet "Use map instead." #-}
-- | Obsolete equivalent of 'map'.
mapSet :: (Ord a, Ord b) => (b -> a) -> Set b -> Set a
mapSet = Data.Set.map

{-# DEPRECATED intersect "Use intersection instead." #-}
-- | Obsolete equivalent of 'intersection'.
intersect :: Ord a => Set a -> Set a -> Set a
intersect = intersection

{-# DEPRECATED addToSet "Use 'flip insert' instead." #-}
-- | Obsolete equivalent of @'flip' 'insert'@.
addToSet :: Ord a => Set a -> a -> Set a
addToSet = flip insert

{-# DEPRECATED delFromSet "Use `flip delete' instead." #-}
-- | Obsolete equivalent of @'flip' 'delete'@.
delFromSet :: Ord a => Set a -> a -> Set a
delFromSet = flip delete
