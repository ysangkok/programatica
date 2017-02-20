module HsLiteral where

-- import Ratio
--import PrettyPrint
--import SrcLoc

data HsLiteral
    = HsInt         Integer
    | HsChar        Char
    | HsString      String
    | HsFrac        Rational
    -- GHC unboxed literals:
    | HsCharPrim    Char
    | HsStringPrim  String
    | HsIntPrim     Integer
    | HsFloatPrim   Rational
    | HsDoublePrim  Rational
    -- GHC extension:
    | HsLitLit      String
      deriving (Eq, Show)
