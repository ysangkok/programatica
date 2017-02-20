module Embed where

data Embedding a b = Emb (a->b) (b->a)

embed (Emb inj proj) f = inj.f.map proj
embed1 (Emb inj proj) f = inj.f.proj
--embedMod (Emb inj proj) f s x = case f s (proj x) of (s',y) -> (s',inj y)
embedMod (Emb inj proj) f = fmap inj . f . proj

idEmb = Emb id id

revEmb (Emb inj proj) = Emb proj inj

--mapEmb (Emb inj proj) = 
