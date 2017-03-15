module HsAssocStruct where

-- Formerly known as InfixAssoc...
data HsFixity = HsFixity HsAssoc Int deriving (Eq,Show,Read)

data HsAssoc
    = HsAssocNone
    | HsAssocLeft
    | HsAssocRight
      deriving (Eq, Show, Read, Ord)
