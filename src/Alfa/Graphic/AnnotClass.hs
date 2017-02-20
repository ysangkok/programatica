module AnnotClass where

{-
-- Haskell does not have multi parameter classes unfortunately...

class IsAnnot a d where
  drawable :: Maybe (d -> Drawing a d)
  build    :: Maybe ([d] -> d)
  menu     :: Maybe (Menu a d)

-}

type Menu a d = [(EditFunc d a,(String,G))]
