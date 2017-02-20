{-| Emulate AgdaIntMap interface on top of library FiniteMap

Implementation by Marcin Benke

-}


module AltIntMap where
import FiniteMap

type IntMap a  = FiniteMap Int a


empty :: IntMap a
empty = emptyFM

add :: (Int, a) -> IntMap a -> IntMap a
add (i, x) im = addToFM im i x

toList :: FiniteMap k a -> [(k,a)]
toList = fmToList

ilookup :: Int -> IntMap a -> Maybe a
ilookup i m = lookupFM m i
{-
instance (Show a,Show b) => Show (FiniteMap a b) where
    showsPrec _ s = showString "{" . f (toList s) . showString "}"
        where f []     = id
              f [x]    = g x 
              f (x:xs) = g x . showString ", " . f xs
              g (i, r) = shows i . showString "->" . shows r
-}
