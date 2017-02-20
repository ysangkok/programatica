module HsKindStruct where

data K x 
    = Kstar    -- base types
    | Kfun !x !x -- higher kinds -- be strict to avoid a space leak
    | Kpred    -- classes
    | Kprop    -- P-logic assertions  & predicates
      deriving (Eq, Show, Read)
