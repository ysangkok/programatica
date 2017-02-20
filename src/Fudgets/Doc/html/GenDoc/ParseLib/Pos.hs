module Pos(Pos,noPos,eofPos,addPos,getPos) where

data Pos a = Pos (Maybe (Int,[a])) -- deriving (Text)

instance Eq (Pos a) where
  Pos Nothing == Pos Nothing = True
  Pos (Just (p1,_)) == Pos (Just (p2,_)) = p1==p2
  _ == _ = False


instance Ord (Pos a) where
  Pos Nothing <= _ = True
  Pos (Just (p1,_)) <= Pos (Just (p2,_)) = p1<=p2
  _ <= _ = False

noPos = Pos Nothing
eofPos = Pos (Just (maxBound,[]))

addPos = add 0
  where
    add n [] = []
    add n tts@(t:ts) = (Pos (Just (n,tts)),t):add (n+1) ts

getPos (Pos Nothing) = (-1,[]) -- ??
getPos (Pos (Just p)) = p
