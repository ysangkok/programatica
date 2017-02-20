-- | Generate unique identifiers (Ints)
module Gensym where
type GenSymId = Int

-- newtype IdSupply  = IdSupply  ([GenSymId],GenSymId)
-- initIdSupply :: IdSupply
-- initIdSupply = IdSupply ([],0)

-- gensymId :: IdSupply -> (GenSymId,IdSupply)
-- gensymId (IdSupply ((i:ids),n)) = (i,IdSupply(ids,n))
-- gensymId (IdSupply (_,n)) = (n,IdSupply ([],n+1))

newtype IdSupply      = IdSupply Int
initIdSupply          = IdSupply 0
gensymId (IdSupply i) = (i, IdSupply (i+1))
