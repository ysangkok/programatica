{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Prelude
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The Prelude: a standard module imported by default into all Haskell
-- modules.  For more documentation, see the Haskell 98 Report
-- <http://www.haskell.org/onlinereport/>.
--
-----------------------------------------------------------------------------

module Prelude ( 
    module Prelude,
    -- * Standard types, classes and related functions

    -- ** Basic data types
    Bool(False, True),
    (&&), (||), not, otherwise,

    Maybe(Nothing, Just),
    maybe,

    Either(Left, Right),
    either,

    Ordering(LT, EQ, GT),
    Char, String,

    -- *** Tuples
    fst, snd, curry, uncurry,

--    (:),		-- Not legal Haskell 98
    
    -- ** Basic type classes
    Eq((==), (/=)),
    Ord(compare, (<), (<=), (>=), (>), max, min),
    Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen,
         enumFromTo, enumFromThenTo),
    Bounded(minBound, maxBound),

    -- ** Numbers

    -- *** Numeric types
    Int, Integer, Float, Double,
    Rational,

    -- *** Numeric type classes
    Num((+), (-), (*), negate, abs, signum, fromInteger),
    Real(toRational),
    Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
    Fractional((/), recip, fromRational),
    Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
             asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
    RealFrac(properFraction, truncate, round, ceiling, floor),
    RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, exponent, significand, scaleFloat, isNaN,
              isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2),

    -- *** Numeric functions
    subtract, even, odd, gcd, lcm, (^), (^^), 
    fromIntegral, realToFrac,

    -- ** Monads and functors
    Monad((>>=), (>>), return, fail),
    Functor(fmap),
    mapM, mapM_, sequence, sequence_, (=<<),

    -- ** Miscellaneous functions
    id, const, (.), flip, ($), until,
    asTypeOf, error, undefined,
    seq, ($!),

    -- * List operations
    map, (++), filter,
    head, last, tail, init, null, length, (!!), 
    reverse,
    -- ** Reducing lists (folds)
    foldl, foldl1, foldr, foldr1,
    -- *** Special folds
    and, or, any, all,
    sum, product,
    concat, concatMap,
    maximum, minimum,
    -- ** Building lists
    -- *** Scans
    scanl, scanl1, scanr, scanr1,
    -- *** Infinite lists
    iterate, repeat, replicate, cycle,
    -- ** Sublists
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    -- ** Searching lists
    elem, notElem, lookup,
    -- ** Zipping and unzipping lists
    zip, zip3, zipWith, zipWith3, unzip, unzip3,
    -- ** Functions on strings
    lines, words, unlines, unwords,

    -- * Converting to and from @String@
    ReadS, ShowS,
    Read(readsPrec, readList),
    Show(showsPrec, showList, show),
    reads, shows, read, lex, 
    showChar, showString, readParen, showParen,
    
    -- * Basic Input and output
    IO,
    -- ** Simple I\/O operations
    -- All I/O functions defined here are character oriented.  The
    -- treatment of the newline character will vary on different systems.
    -- For example, two characters of input, return and linefeed, may
    -- read as a single newline character.  These functions cannot be
    -- used portably for binary I/O.
    -- *** Output functions
    putChar,
    putStr, putStrLn, print,
    -- *** Input functions
    getChar,
    getLine, getContents, interact,
    -- *** Files
    FilePath,
    readFile, writeFile, appendFile, readIO, readLn,
    -- ** Exception handling in the I\/O monad
    IOError, ioError, userError, catch

  ) where

import Hugs.Prelude

infixr 5  :    -- this fixity declaration is hard-wired into Hugs

data [a] = [] | a : [a] deriving (Eq, Ord)

data (->) a b

data () = () deriving (Eq, Ord, {-Ix,-} Enum, Read, Show, Bounded)
data (a,b) = (,) a b deriving (Eq, Ord, {-Ix,-} Read, Show, Bounded)
data (a,b,c) = (,,) a b c deriving (Eq, Ord, {-Ix,-} Read, Show, Bounded)
data (a,b,c,d) = (,,,) a b c d deriving (Eq, Ord, {-Ix,-} Read, Show, Bounded)
data (a,b,c,d,e) = (,,,,) a b c d e deriving (Eq, Ord, {-Ix,-} Read, Show, Bounded)
data (a,b,c,d,e,f) = (,,,,,) a b c d e f
     deriving (Eq, Ord, {-Ix,-} Read, Show, Bounded)
data (a,b,c,d,e,f,g) = (,,,,,,) a b c d e f g
     deriving (Eq, Ord, {-Ix,-} Read, Show, Bounded)
data (a,b,c,d,e,f,g,h) = (,,,,,,,) a b c d e f g h
     deriving (Eq, Ord, {-Ix,-} Read, Show, Bounded)
data (a,b,c,d,e,f,g,h,i) = (,,,,,,,,) a b c d e f g h i
     deriving (Eq, Ord, {-Ix,-} Read, Show, Bounded)
data (a,b,c,d,e,f,g,h,i,j) = (,,,,,,,,,) a b c d e f g h i j
     deriving (Eq, Ord, {-Ix,-} Read, Show, Bounded)
data (a,b,c,d,e,f,g,h,i,j,k) = (,,,,,,,,,,) a b c d e f g h i j k 
     deriving (Eq, Ord, {-Ix,-} Read, Show, Bounded)
data (a,b,c,d,e,f,g,h,i,j,k,l) = (,,,,,,,,,,,) a b c d e f g h i j k l
     deriving (Eq, Ord, {-Ix,-} Read, Show, Bounded)
data (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = (,,,,,,,,,,,,,) a b c d e f g h i j k l m n
     deriving (Eq, Ord, {-Ix,-} Read, Show, Bounded)


-- Extra definitions for PFE -------------------------------------------------

-- For use in derived Ord instances:
lexOrder EQ o = o
lexOrder o  _ = o


-- For use in derived Read instances:
readToken x t s = [(x,r)|(t',r)<-lex s,t'==t]

readParenArg :: Int -> ReadS a -> ReadS a
readParenArg d = readParen (10<=d)
readArgument s = readsPrec 10 s

rf `readAp` rx = \ s0 -> [(f x,s2) | (f,s1)<-rf s0,(x,s2)<-rx s1]

readChoice rd1 rd2 s = rd1 s ++ rd2 s


-- For use in derived Show instances:
showParenArg :: Int -> ShowS -> ShowS
showParenArg d = showParen (10<=d)

showArgument x = showChar ' ' . showsPrec 10 x

