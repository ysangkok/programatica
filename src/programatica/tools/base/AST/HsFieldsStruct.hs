-- Labelled fields, used in patterns and expressions
module HsFieldsStruct where

type HsFieldsI i e = [HsFieldI i e]

data HsFieldI i e = HsField i e deriving (Eq, Show)   

fieldName (HsField n _) = n
