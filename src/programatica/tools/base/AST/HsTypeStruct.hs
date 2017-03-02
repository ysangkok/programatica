module HsTypeStruct where

-------- Types -----------------------------------------------------------------

data TI i t  
    = HsTyFun t t
--  | HsTyTuple [t]
    | HsTyApp t t
    | HsTyVar i
    | HsTyCon i
    | HsTyForall [i] [t] t -- forall is . Ps => t
      deriving (Eq,Show,Read,Ord)
