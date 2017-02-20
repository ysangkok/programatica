module Dynamic where

class Dynamic a where
  toDyn :: a -> Dyn
  fromDyn :: Dyn -> a

typeTag :: Dyn -> Type
typeTag (Dyn t _) = t

type Type = String

data Dyn = (Dynamic a) => Dyn Type a

instance Dynamic Char where
  toDyn c = Dyn "Char" c
  fromDyn (Dyn _ c) = c
