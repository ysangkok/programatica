{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Types
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX data types: Haskell equivalents of the types defined by the
-- @\<sys\/types.h>@ C header on a POSIX system.
--
-----------------------------------------------------------------------------

module System.Posix.Types (

  -- * POSIX data types

  CDev,

  CIno,

  CMode,

  COff,

  CPid,

  CSsize,

  CGid,

  CNlink,

  CUid,

  CCc,

  CSpeed,

  CTcflag,

  CRLim,

  Fd(..),

  LinkCount,

  UserID,

  GroupID,

  ByteCount,
  ClockTick,
  EpochTime,
  FileOffset,
  ProcessID,
  ProcessGroupID,
  DeviceID,
  FileID,
  FileMode,
  Limit
 ) where

import Foreign
import Foreign.C
import Data.Typeable
import Data.Bits

import Control.Monad

newtype CDev = CDev Word64 deriving (Eq, Ord) ; instance Num CDev where {    (CDev i) + (CDev j) = CDev (i + j) ;    (CDev i) - (CDev j) = CDev (i - j) ;    (CDev i) * (CDev j) = CDev (i * j) ;    negate  (CDev i) = CDev (negate i) ;    abs     (CDev i) = CDev (abs    i) ;    signum  (CDev i) = CDev (signum i) ;    fromInteger x = CDev (fromInteger x) } ; instance Read CDev where {    readsPrec p s = map (\(x, t) -> (CDev x, t)) (readsPrec p s) } ; instance Show CDev where {    showsPrec p (CDev x) = showsPrec p x } ; instance Enum CDev where {    succ           (CDev i)             = CDev (succ i) ;    pred           (CDev i)             = CDev (pred i) ;    toEnum               x           = CDev (toEnum x) ;    fromEnum       (CDev i)             = fromEnum i ;    enumFrom       (CDev i)             = map CDev (enumFrom i) ;    enumFromThen   (CDev i) (CDev j)       = map CDev (enumFromThen i j) ;    enumFromTo     (CDev i) (CDev j)       = map CDev (enumFromTo i j) ;    enumFromThenTo (CDev i) (CDev j) (CDev k) = map CDev (enumFromThenTo i j k) } ; instance Storable CDev where {    sizeOf    (CDev x)       = sizeOf x ;    alignment (CDev x)       = alignment x ;    peekElemOff a i       = liftM CDev (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CDev x) = pokeElemOff (castPtr a) i x } ; tyConCDev = mkTyCon "CDev"; instance Typeable CDev where { typeOf _ = mkAppTy tyConCDev [] } ;

newtype CIno = CIno Word32 deriving (Eq, Ord) ; instance Num CIno where {    (CIno i) + (CIno j) = CIno (i + j) ;    (CIno i) - (CIno j) = CIno (i - j) ;    (CIno i) * (CIno j) = CIno (i * j) ;    negate  (CIno i) = CIno (negate i) ;    abs     (CIno i) = CIno (abs    i) ;    signum  (CIno i) = CIno (signum i) ;    fromInteger x = CIno (fromInteger x) } ; instance Read CIno where {    readsPrec p s = map (\(x, t) -> (CIno x, t)) (readsPrec p s) } ; instance Show CIno where {    showsPrec p (CIno x) = showsPrec p x } ; instance Enum CIno where {    succ           (CIno i)             = CIno (succ i) ;    pred           (CIno i)             = CIno (pred i) ;    toEnum               x           = CIno (toEnum x) ;    fromEnum       (CIno i)             = fromEnum i ;    enumFrom       (CIno i)             = map CIno (enumFrom i) ;    enumFromThen   (CIno i) (CIno j)       = map CIno (enumFromThen i j) ;    enumFromTo     (CIno i) (CIno j)       = map CIno (enumFromTo i j) ;    enumFromThenTo (CIno i) (CIno j) (CIno k) = map CIno (enumFromThenTo i j k) } ; instance Storable CIno where {    sizeOf    (CIno x)       = sizeOf x ;    alignment (CIno x)       = alignment x ;    peekElemOff a i       = liftM CIno (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CIno x) = pokeElemOff (castPtr a) i x } ; tyConCIno = mkTyCon "CIno"; instance Typeable CIno where { typeOf _ = mkAppTy tyConCIno [] } ; ; instance Bounded CIno where {    minBound = CIno minBound ;    maxBound = CIno maxBound } ; instance Real CIno where {    toRational (CIno i) = toRational i } ; instance Integral CIno where {    (CIno i) `quot`    (CIno j) = CIno (i `quot` j) ;    (CIno i) `rem`     (CIno j) = CIno (i `rem`  j) ;    (CIno i) `div`     (CIno j) = CIno (i `div`  j) ;    (CIno i) `mod`     (CIno j) = CIno (i `mod`  j) ;    (CIno i) `quotRem` (CIno j) = let (q,r) = i `quotRem` j in (CIno q, CIno r) ;    (CIno i) `divMod`  (CIno j) = let (d,m) = i `divMod`  j in (CIno d, CIno m) ;    toInteger (CIno i)       = toInteger i } ; instance Bits CIno where {   (CIno x) .&.     (CIno y)   = CIno (x .&.   y) ;   (CIno x) .|.     (CIno y)   = CIno (x .|.   y) ;   (CIno x) `xor`   (CIno y)   = CIno (x `xor` y) ;   complement    (CIno x)   = CIno (complement x) ;   shift         (CIno x) n = CIno (shift x n) ;   rotate        (CIno x) n = CIno (rotate x n) ;   bit                 n = CIno (bit n) ;   setBit        (CIno x) n = CIno (setBit x n) ;   clearBit      (CIno x) n = CIno (clearBit x n) ;   complementBit (CIno x) n = CIno (complementBit x n) ;   testBit       (CIno x) n = testBit x n ;   bitSize       (CIno x)   = bitSize x ;   isSigned      (CIno x)   = isSigned x }

newtype CMode = CMode Word32 deriving (Eq, Ord) ; instance Num CMode where {    (CMode i) + (CMode j) = CMode (i + j) ;    (CMode i) - (CMode j) = CMode (i - j) ;    (CMode i) * (CMode j) = CMode (i * j) ;    negate  (CMode i) = CMode (negate i) ;    abs     (CMode i) = CMode (abs    i) ;    signum  (CMode i) = CMode (signum i) ;    fromInteger x = CMode (fromInteger x) } ; instance Read CMode where {    readsPrec p s = map (\(x, t) -> (CMode x, t)) (readsPrec p s) } ; instance Show CMode where {    showsPrec p (CMode x) = showsPrec p x } ; instance Enum CMode where {    succ           (CMode i)             = CMode (succ i) ;    pred           (CMode i)             = CMode (pred i) ;    toEnum               x           = CMode (toEnum x) ;    fromEnum       (CMode i)             = fromEnum i ;    enumFrom       (CMode i)             = map CMode (enumFrom i) ;    enumFromThen   (CMode i) (CMode j)       = map CMode (enumFromThen i j) ;    enumFromTo     (CMode i) (CMode j)       = map CMode (enumFromTo i j) ;    enumFromThenTo (CMode i) (CMode j) (CMode k) = map CMode (enumFromThenTo i j k) } ; instance Storable CMode where {    sizeOf    (CMode x)       = sizeOf x ;    alignment (CMode x)       = alignment x ;    peekElemOff a i       = liftM CMode (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CMode x) = pokeElemOff (castPtr a) i x } ; tyConCMode = mkTyCon "CMode"; instance Typeable CMode where { typeOf _ = mkAppTy tyConCMode [] } ; ; instance Bounded CMode where {    minBound = CMode minBound ;    maxBound = CMode maxBound } ; instance Real CMode where {    toRational (CMode i) = toRational i } ; instance Integral CMode where {    (CMode i) `quot`    (CMode j) = CMode (i `quot` j) ;    (CMode i) `rem`     (CMode j) = CMode (i `rem`  j) ;    (CMode i) `div`     (CMode j) = CMode (i `div`  j) ;    (CMode i) `mod`     (CMode j) = CMode (i `mod`  j) ;    (CMode i) `quotRem` (CMode j) = let (q,r) = i `quotRem` j in (CMode q, CMode r) ;    (CMode i) `divMod`  (CMode j) = let (d,m) = i `divMod`  j in (CMode d, CMode m) ;    toInteger (CMode i)       = toInteger i } ; instance Bits CMode where {   (CMode x) .&.     (CMode y)   = CMode (x .&.   y) ;   (CMode x) .|.     (CMode y)   = CMode (x .|.   y) ;   (CMode x) `xor`   (CMode y)   = CMode (x `xor` y) ;   complement    (CMode x)   = CMode (complement x) ;   shift         (CMode x) n = CMode (shift x n) ;   rotate        (CMode x) n = CMode (rotate x n) ;   bit                 n = CMode (bit n) ;   setBit        (CMode x) n = CMode (setBit x n) ;   clearBit      (CMode x) n = CMode (clearBit x n) ;   complementBit (CMode x) n = CMode (complementBit x n) ;   testBit       (CMode x) n = testBit x n ;   bitSize       (CMode x)   = bitSize x ;   isSigned      (CMode x)   = isSigned x }

newtype COff = COff Int32 deriving (Eq, Ord) ; instance Num COff where {    (COff i) + (COff j) = COff (i + j) ;    (COff i) - (COff j) = COff (i - j) ;    (COff i) * (COff j) = COff (i * j) ;    negate  (COff i) = COff (negate i) ;    abs     (COff i) = COff (abs    i) ;    signum  (COff i) = COff (signum i) ;    fromInteger x = COff (fromInteger x) } ; instance Read COff where {    readsPrec p s = map (\(x, t) -> (COff x, t)) (readsPrec p s) } ; instance Show COff where {    showsPrec p (COff x) = showsPrec p x } ; instance Enum COff where {    succ           (COff i)             = COff (succ i) ;    pred           (COff i)             = COff (pred i) ;    toEnum               x           = COff (toEnum x) ;    fromEnum       (COff i)             = fromEnum i ;    enumFrom       (COff i)             = map COff (enumFrom i) ;    enumFromThen   (COff i) (COff j)       = map COff (enumFromThen i j) ;    enumFromTo     (COff i) (COff j)       = map COff (enumFromTo i j) ;    enumFromThenTo (COff i) (COff j) (COff k) = map COff (enumFromThenTo i j k) } ; instance Storable COff where {    sizeOf    (COff x)       = sizeOf x ;    alignment (COff x)       = alignment x ;    peekElemOff a i       = liftM COff (peekElemOff (castPtr a) i) ;    pokeElemOff a i (COff x) = pokeElemOff (castPtr a) i x } ; tyConCOff = mkTyCon "COff"; instance Typeable COff where { typeOf _ = mkAppTy tyConCOff [] } ; ; instance Bounded COff where {    minBound = COff minBound ;    maxBound = COff maxBound } ; instance Real COff where {    toRational (COff i) = toRational i } ; instance Integral COff where {    (COff i) `quot`    (COff j) = COff (i `quot` j) ;    (COff i) `rem`     (COff j) = COff (i `rem`  j) ;    (COff i) `div`     (COff j) = COff (i `div`  j) ;    (COff i) `mod`     (COff j) = COff (i `mod`  j) ;    (COff i) `quotRem` (COff j) = let (q,r) = i `quotRem` j in (COff q, COff r) ;    (COff i) `divMod`  (COff j) = let (d,m) = i `divMod`  j in (COff d, COff m) ;    toInteger (COff i)       = toInteger i } ; instance Bits COff where {   (COff x) .&.     (COff y)   = COff (x .&.   y) ;   (COff x) .|.     (COff y)   = COff (x .|.   y) ;   (COff x) `xor`   (COff y)   = COff (x `xor` y) ;   complement    (COff x)   = COff (complement x) ;   shift         (COff x) n = COff (shift x n) ;   rotate        (COff x) n = COff (rotate x n) ;   bit                 n = COff (bit n) ;   setBit        (COff x) n = COff (setBit x n) ;   clearBit      (COff x) n = COff (clearBit x n) ;   complementBit (COff x) n = COff (complementBit x n) ;   testBit       (COff x) n = testBit x n ;   bitSize       (COff x)   = bitSize x ;   isSigned      (COff x)   = isSigned x }

newtype CPid = CPid Int32 deriving (Eq, Ord) ; instance Num CPid where {    (CPid i) + (CPid j) = CPid (i + j) ;    (CPid i) - (CPid j) = CPid (i - j) ;    (CPid i) * (CPid j) = CPid (i * j) ;    negate  (CPid i) = CPid (negate i) ;    abs     (CPid i) = CPid (abs    i) ;    signum  (CPid i) = CPid (signum i) ;    fromInteger x = CPid (fromInteger x) } ; instance Read CPid where {    readsPrec p s = map (\(x, t) -> (CPid x, t)) (readsPrec p s) } ; instance Show CPid where {    showsPrec p (CPid x) = showsPrec p x } ; instance Enum CPid where {    succ           (CPid i)             = CPid (succ i) ;    pred           (CPid i)             = CPid (pred i) ;    toEnum               x           = CPid (toEnum x) ;    fromEnum       (CPid i)             = fromEnum i ;    enumFrom       (CPid i)             = map CPid (enumFrom i) ;    enumFromThen   (CPid i) (CPid j)       = map CPid (enumFromThen i j) ;    enumFromTo     (CPid i) (CPid j)       = map CPid (enumFromTo i j) ;    enumFromThenTo (CPid i) (CPid j) (CPid k) = map CPid (enumFromThenTo i j k) } ; instance Storable CPid where {    sizeOf    (CPid x)       = sizeOf x ;    alignment (CPid x)       = alignment x ;    peekElemOff a i       = liftM CPid (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CPid x) = pokeElemOff (castPtr a) i x } ; tyConCPid = mkTyCon "CPid"; instance Typeable CPid where { typeOf _ = mkAppTy tyConCPid [] } ; ; instance Bounded CPid where {    minBound = CPid minBound ;    maxBound = CPid maxBound } ; instance Real CPid where {    toRational (CPid i) = toRational i } ; instance Integral CPid where {    (CPid i) `quot`    (CPid j) = CPid (i `quot` j) ;    (CPid i) `rem`     (CPid j) = CPid (i `rem`  j) ;    (CPid i) `div`     (CPid j) = CPid (i `div`  j) ;    (CPid i) `mod`     (CPid j) = CPid (i `mod`  j) ;    (CPid i) `quotRem` (CPid j) = let (q,r) = i `quotRem` j in (CPid q, CPid r) ;    (CPid i) `divMod`  (CPid j) = let (d,m) = i `divMod`  j in (CPid d, CPid m) ;    toInteger (CPid i)       = toInteger i } ; instance Bits CPid where {   (CPid x) .&.     (CPid y)   = CPid (x .&.   y) ;   (CPid x) .|.     (CPid y)   = CPid (x .|.   y) ;   (CPid x) `xor`   (CPid y)   = CPid (x `xor` y) ;   complement    (CPid x)   = CPid (complement x) ;   shift         (CPid x) n = CPid (shift x n) ;   rotate        (CPid x) n = CPid (rotate x n) ;   bit                 n = CPid (bit n) ;   setBit        (CPid x) n = CPid (setBit x n) ;   clearBit      (CPid x) n = CPid (clearBit x n) ;   complementBit (CPid x) n = CPid (complementBit x n) ;   testBit       (CPid x) n = testBit x n ;   bitSize       (CPid x)   = bitSize x ;   isSigned      (CPid x)   = isSigned x }

newtype CSsize = CSsize Word32 deriving (Eq, Ord) ; instance Num CSsize where {    (CSsize i) + (CSsize j) = CSsize (i + j) ;    (CSsize i) - (CSsize j) = CSsize (i - j) ;    (CSsize i) * (CSsize j) = CSsize (i * j) ;    negate  (CSsize i) = CSsize (negate i) ;    abs     (CSsize i) = CSsize (abs    i) ;    signum  (CSsize i) = CSsize (signum i) ;    fromInteger x = CSsize (fromInteger x) } ; instance Read CSsize where {    readsPrec p s = map (\(x, t) -> (CSsize x, t)) (readsPrec p s) } ; instance Show CSsize where {    showsPrec p (CSsize x) = showsPrec p x } ; instance Enum CSsize where {    succ           (CSsize i)             = CSsize (succ i) ;    pred           (CSsize i)             = CSsize (pred i) ;    toEnum               x           = CSsize (toEnum x) ;    fromEnum       (CSsize i)             = fromEnum i ;    enumFrom       (CSsize i)             = map CSsize (enumFrom i) ;    enumFromThen   (CSsize i) (CSsize j)       = map CSsize (enumFromThen i j) ;    enumFromTo     (CSsize i) (CSsize j)       = map CSsize (enumFromTo i j) ;    enumFromThenTo (CSsize i) (CSsize j) (CSsize k) = map CSsize (enumFromThenTo i j k) } ; instance Storable CSsize where {    sizeOf    (CSsize x)       = sizeOf x ;    alignment (CSsize x)       = alignment x ;    peekElemOff a i       = liftM CSsize (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CSsize x) = pokeElemOff (castPtr a) i x } ; tyConCSsize = mkTyCon "CSsize"; instance Typeable CSsize where { typeOf _ = mkAppTy tyConCSsize [] } ; ; instance Bounded CSsize where {    minBound = CSsize minBound ;    maxBound = CSsize maxBound } ; instance Real CSsize where {    toRational (CSsize i) = toRational i } ; instance Integral CSsize where {    (CSsize i) `quot`    (CSsize j) = CSsize (i `quot` j) ;    (CSsize i) `rem`     (CSsize j) = CSsize (i `rem`  j) ;    (CSsize i) `div`     (CSsize j) = CSsize (i `div`  j) ;    (CSsize i) `mod`     (CSsize j) = CSsize (i `mod`  j) ;    (CSsize i) `quotRem` (CSsize j) = let (q,r) = i `quotRem` j in (CSsize q, CSsize r) ;    (CSsize i) `divMod`  (CSsize j) = let (d,m) = i `divMod`  j in (CSsize d, CSsize m) ;    toInteger (CSsize i)       = toInteger i } ; instance Bits CSsize where {   (CSsize x) .&.     (CSsize y)   = CSsize (x .&.   y) ;   (CSsize x) .|.     (CSsize y)   = CSsize (x .|.   y) ;   (CSsize x) `xor`   (CSsize y)   = CSsize (x `xor` y) ;   complement    (CSsize x)   = CSsize (complement x) ;   shift         (CSsize x) n = CSsize (shift x n) ;   rotate        (CSsize x) n = CSsize (rotate x n) ;   bit                 n = CSsize (bit n) ;   setBit        (CSsize x) n = CSsize (setBit x n) ;   clearBit      (CSsize x) n = CSsize (clearBit x n) ;   complementBit (CSsize x) n = CSsize (complementBit x n) ;   testBit       (CSsize x) n = testBit x n ;   bitSize       (CSsize x)   = bitSize x ;   isSigned      (CSsize x)   = isSigned x }

newtype CGid = CGid Word32 deriving (Eq, Ord) ; instance Num CGid where {    (CGid i) + (CGid j) = CGid (i + j) ;    (CGid i) - (CGid j) = CGid (i - j) ;    (CGid i) * (CGid j) = CGid (i * j) ;    negate  (CGid i) = CGid (negate i) ;    abs     (CGid i) = CGid (abs    i) ;    signum  (CGid i) = CGid (signum i) ;    fromInteger x = CGid (fromInteger x) } ; instance Read CGid where {    readsPrec p s = map (\(x, t) -> (CGid x, t)) (readsPrec p s) } ; instance Show CGid where {    showsPrec p (CGid x) = showsPrec p x } ; instance Enum CGid where {    succ           (CGid i)             = CGid (succ i) ;    pred           (CGid i)             = CGid (pred i) ;    toEnum               x           = CGid (toEnum x) ;    fromEnum       (CGid i)             = fromEnum i ;    enumFrom       (CGid i)             = map CGid (enumFrom i) ;    enumFromThen   (CGid i) (CGid j)       = map CGid (enumFromThen i j) ;    enumFromTo     (CGid i) (CGid j)       = map CGid (enumFromTo i j) ;    enumFromThenTo (CGid i) (CGid j) (CGid k) = map CGid (enumFromThenTo i j k) } ; instance Storable CGid where {    sizeOf    (CGid x)       = sizeOf x ;    alignment (CGid x)       = alignment x ;    peekElemOff a i       = liftM CGid (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CGid x) = pokeElemOff (castPtr a) i x } ; tyConCGid = mkTyCon "CGid"; instance Typeable CGid where { typeOf _ = mkAppTy tyConCGid [] } ; ; instance Bounded CGid where {    minBound = CGid minBound ;    maxBound = CGid maxBound } ; instance Real CGid where {    toRational (CGid i) = toRational i } ; instance Integral CGid where {    (CGid i) `quot`    (CGid j) = CGid (i `quot` j) ;    (CGid i) `rem`     (CGid j) = CGid (i `rem`  j) ;    (CGid i) `div`     (CGid j) = CGid (i `div`  j) ;    (CGid i) `mod`     (CGid j) = CGid (i `mod`  j) ;    (CGid i) `quotRem` (CGid j) = let (q,r) = i `quotRem` j in (CGid q, CGid r) ;    (CGid i) `divMod`  (CGid j) = let (d,m) = i `divMod`  j in (CGid d, CGid m) ;    toInteger (CGid i)       = toInteger i } ; instance Bits CGid where {   (CGid x) .&.     (CGid y)   = CGid (x .&.   y) ;   (CGid x) .|.     (CGid y)   = CGid (x .|.   y) ;   (CGid x) `xor`   (CGid y)   = CGid (x `xor` y) ;   complement    (CGid x)   = CGid (complement x) ;   shift         (CGid x) n = CGid (shift x n) ;   rotate        (CGid x) n = CGid (rotate x n) ;   bit                 n = CGid (bit n) ;   setBit        (CGid x) n = CGid (setBit x n) ;   clearBit      (CGid x) n = CGid (clearBit x n) ;   complementBit (CGid x) n = CGid (complementBit x n) ;   testBit       (CGid x) n = testBit x n ;   bitSize       (CGid x)   = bitSize x ;   isSigned      (CGid x)   = isSigned x }

newtype CNlink = CNlink Word32 deriving (Eq, Ord) ; instance Num CNlink where {    (CNlink i) + (CNlink j) = CNlink (i + j) ;    (CNlink i) - (CNlink j) = CNlink (i - j) ;    (CNlink i) * (CNlink j) = CNlink (i * j) ;    negate  (CNlink i) = CNlink (negate i) ;    abs     (CNlink i) = CNlink (abs    i) ;    signum  (CNlink i) = CNlink (signum i) ;    fromInteger x = CNlink (fromInteger x) } ; instance Read CNlink where {    readsPrec p s = map (\(x, t) -> (CNlink x, t)) (readsPrec p s) } ; instance Show CNlink where {    showsPrec p (CNlink x) = showsPrec p x } ; instance Enum CNlink where {    succ           (CNlink i)             = CNlink (succ i) ;    pred           (CNlink i)             = CNlink (pred i) ;    toEnum               x           = CNlink (toEnum x) ;    fromEnum       (CNlink i)             = fromEnum i ;    enumFrom       (CNlink i)             = map CNlink (enumFrom i) ;    enumFromThen   (CNlink i) (CNlink j)       = map CNlink (enumFromThen i j) ;    enumFromTo     (CNlink i) (CNlink j)       = map CNlink (enumFromTo i j) ;    enumFromThenTo (CNlink i) (CNlink j) (CNlink k) = map CNlink (enumFromThenTo i j k) } ; instance Storable CNlink where {    sizeOf    (CNlink x)       = sizeOf x ;    alignment (CNlink x)       = alignment x ;    peekElemOff a i       = liftM CNlink (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CNlink x) = pokeElemOff (castPtr a) i x } ; tyConCNlink = mkTyCon "CNlink"; instance Typeable CNlink where { typeOf _ = mkAppTy tyConCNlink [] } ; ; instance Bounded CNlink where {    minBound = CNlink minBound ;    maxBound = CNlink maxBound } ; instance Real CNlink where {    toRational (CNlink i) = toRational i } ; instance Integral CNlink where {    (CNlink i) `quot`    (CNlink j) = CNlink (i `quot` j) ;    (CNlink i) `rem`     (CNlink j) = CNlink (i `rem`  j) ;    (CNlink i) `div`     (CNlink j) = CNlink (i `div`  j) ;    (CNlink i) `mod`     (CNlink j) = CNlink (i `mod`  j) ;    (CNlink i) `quotRem` (CNlink j) = let (q,r) = i `quotRem` j in (CNlink q, CNlink r) ;    (CNlink i) `divMod`  (CNlink j) = let (d,m) = i `divMod`  j in (CNlink d, CNlink m) ;    toInteger (CNlink i)       = toInteger i } ; instance Bits CNlink where {   (CNlink x) .&.     (CNlink y)   = CNlink (x .&.   y) ;   (CNlink x) .|.     (CNlink y)   = CNlink (x .|.   y) ;   (CNlink x) `xor`   (CNlink y)   = CNlink (x `xor` y) ;   complement    (CNlink x)   = CNlink (complement x) ;   shift         (CNlink x) n = CNlink (shift x n) ;   rotate        (CNlink x) n = CNlink (rotate x n) ;   bit                 n = CNlink (bit n) ;   setBit        (CNlink x) n = CNlink (setBit x n) ;   clearBit      (CNlink x) n = CNlink (clearBit x n) ;   complementBit (CNlink x) n = CNlink (complementBit x n) ;   testBit       (CNlink x) n = testBit x n ;   bitSize       (CNlink x)   = bitSize x ;   isSigned      (CNlink x)   = isSigned x }

newtype CUid = CUid Word32 deriving (Eq, Ord) ; instance Num CUid where {    (CUid i) + (CUid j) = CUid (i + j) ;    (CUid i) - (CUid j) = CUid (i - j) ;    (CUid i) * (CUid j) = CUid (i * j) ;    negate  (CUid i) = CUid (negate i) ;    abs     (CUid i) = CUid (abs    i) ;    signum  (CUid i) = CUid (signum i) ;    fromInteger x = CUid (fromInteger x) } ; instance Read CUid where {    readsPrec p s = map (\(x, t) -> (CUid x, t)) (readsPrec p s) } ; instance Show CUid where {    showsPrec p (CUid x) = showsPrec p x } ; instance Enum CUid where {    succ           (CUid i)             = CUid (succ i) ;    pred           (CUid i)             = CUid (pred i) ;    toEnum               x           = CUid (toEnum x) ;    fromEnum       (CUid i)             = fromEnum i ;    enumFrom       (CUid i)             = map CUid (enumFrom i) ;    enumFromThen   (CUid i) (CUid j)       = map CUid (enumFromThen i j) ;    enumFromTo     (CUid i) (CUid j)       = map CUid (enumFromTo i j) ;    enumFromThenTo (CUid i) (CUid j) (CUid k) = map CUid (enumFromThenTo i j k) } ; instance Storable CUid where {    sizeOf    (CUid x)       = sizeOf x ;    alignment (CUid x)       = alignment x ;    peekElemOff a i       = liftM CUid (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CUid x) = pokeElemOff (castPtr a) i x } ; tyConCUid = mkTyCon "CUid"; instance Typeable CUid where { typeOf _ = mkAppTy tyConCUid [] } ; ; instance Bounded CUid where {    minBound = CUid minBound ;    maxBound = CUid maxBound } ; instance Real CUid where {    toRational (CUid i) = toRational i } ; instance Integral CUid where {    (CUid i) `quot`    (CUid j) = CUid (i `quot` j) ;    (CUid i) `rem`     (CUid j) = CUid (i `rem`  j) ;    (CUid i) `div`     (CUid j) = CUid (i `div`  j) ;    (CUid i) `mod`     (CUid j) = CUid (i `mod`  j) ;    (CUid i) `quotRem` (CUid j) = let (q,r) = i `quotRem` j in (CUid q, CUid r) ;    (CUid i) `divMod`  (CUid j) = let (d,m) = i `divMod`  j in (CUid d, CUid m) ;    toInteger (CUid i)       = toInteger i } ; instance Bits CUid where {   (CUid x) .&.     (CUid y)   = CUid (x .&.   y) ;   (CUid x) .|.     (CUid y)   = CUid (x .|.   y) ;   (CUid x) `xor`   (CUid y)   = CUid (x `xor` y) ;   complement    (CUid x)   = CUid (complement x) ;   shift         (CUid x) n = CUid (shift x n) ;   rotate        (CUid x) n = CUid (rotate x n) ;   bit                 n = CUid (bit n) ;   setBit        (CUid x) n = CUid (setBit x n) ;   clearBit      (CUid x) n = CUid (clearBit x n) ;   complementBit (CUid x) n = CUid (complementBit x n) ;   testBit       (CUid x) n = testBit x n ;   bitSize       (CUid x)   = bitSize x ;   isSigned      (CUid x)   = isSigned x }

newtype CCc = CCc Word8 deriving (Eq, Ord) ; instance Num CCc where {    (CCc i) + (CCc j) = CCc (i + j) ;    (CCc i) - (CCc j) = CCc (i - j) ;    (CCc i) * (CCc j) = CCc (i * j) ;    negate  (CCc i) = CCc (negate i) ;    abs     (CCc i) = CCc (abs    i) ;    signum  (CCc i) = CCc (signum i) ;    fromInteger x = CCc (fromInteger x) } ; instance Read CCc where {    readsPrec p s = map (\(x, t) -> (CCc x, t)) (readsPrec p s) } ; instance Show CCc where {    showsPrec p (CCc x) = showsPrec p x } ; instance Enum CCc where {    succ           (CCc i)             = CCc (succ i) ;    pred           (CCc i)             = CCc (pred i) ;    toEnum               x           = CCc (toEnum x) ;    fromEnum       (CCc i)             = fromEnum i ;    enumFrom       (CCc i)             = map CCc (enumFrom i) ;    enumFromThen   (CCc i) (CCc j)       = map CCc (enumFromThen i j) ;    enumFromTo     (CCc i) (CCc j)       = map CCc (enumFromTo i j) ;    enumFromThenTo (CCc i) (CCc j) (CCc k) = map CCc (enumFromThenTo i j k) } ; instance Storable CCc where {    sizeOf    (CCc x)       = sizeOf x ;    alignment (CCc x)       = alignment x ;    peekElemOff a i       = liftM CCc (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CCc x) = pokeElemOff (castPtr a) i x } ; tyConCCc = mkTyCon "CCc"; instance Typeable CCc where { typeOf _ = mkAppTy tyConCCc [] } ;

newtype CSpeed = CSpeed Word32 deriving (Eq, Ord) ; instance Num CSpeed where {    (CSpeed i) + (CSpeed j) = CSpeed (i + j) ;    (CSpeed i) - (CSpeed j) = CSpeed (i - j) ;    (CSpeed i) * (CSpeed j) = CSpeed (i * j) ;    negate  (CSpeed i) = CSpeed (negate i) ;    abs     (CSpeed i) = CSpeed (abs    i) ;    signum  (CSpeed i) = CSpeed (signum i) ;    fromInteger x = CSpeed (fromInteger x) } ; instance Read CSpeed where {    readsPrec p s = map (\(x, t) -> (CSpeed x, t)) (readsPrec p s) } ; instance Show CSpeed where {    showsPrec p (CSpeed x) = showsPrec p x } ; instance Enum CSpeed where {    succ           (CSpeed i)             = CSpeed (succ i) ;    pred           (CSpeed i)             = CSpeed (pred i) ;    toEnum               x           = CSpeed (toEnum x) ;    fromEnum       (CSpeed i)             = fromEnum i ;    enumFrom       (CSpeed i)             = map CSpeed (enumFrom i) ;    enumFromThen   (CSpeed i) (CSpeed j)       = map CSpeed (enumFromThen i j) ;    enumFromTo     (CSpeed i) (CSpeed j)       = map CSpeed (enumFromTo i j) ;    enumFromThenTo (CSpeed i) (CSpeed j) (CSpeed k) = map CSpeed (enumFromThenTo i j k) } ; instance Storable CSpeed where {    sizeOf    (CSpeed x)       = sizeOf x ;    alignment (CSpeed x)       = alignment x ;    peekElemOff a i       = liftM CSpeed (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CSpeed x) = pokeElemOff (castPtr a) i x } ; tyConCSpeed = mkTyCon "CSpeed"; instance Typeable CSpeed where { typeOf _ = mkAppTy tyConCSpeed [] } ;

newtype CTcflag = CTcflag Word32 deriving (Eq, Ord) ; instance Num CTcflag where {    (CTcflag i) + (CTcflag j) = CTcflag (i + j) ;    (CTcflag i) - (CTcflag j) = CTcflag (i - j) ;    (CTcflag i) * (CTcflag j) = CTcflag (i * j) ;    negate  (CTcflag i) = CTcflag (negate i) ;    abs     (CTcflag i) = CTcflag (abs    i) ;    signum  (CTcflag i) = CTcflag (signum i) ;    fromInteger x = CTcflag (fromInteger x) } ; instance Read CTcflag where {    readsPrec p s = map (\(x, t) -> (CTcflag x, t)) (readsPrec p s) } ; instance Show CTcflag where {    showsPrec p (CTcflag x) = showsPrec p x } ; instance Enum CTcflag where {    succ           (CTcflag i)             = CTcflag (succ i) ;    pred           (CTcflag i)             = CTcflag (pred i) ;    toEnum               x           = CTcflag (toEnum x) ;    fromEnum       (CTcflag i)             = fromEnum i ;    enumFrom       (CTcflag i)             = map CTcflag (enumFrom i) ;    enumFromThen   (CTcflag i) (CTcflag j)       = map CTcflag (enumFromThen i j) ;    enumFromTo     (CTcflag i) (CTcflag j)       = map CTcflag (enumFromTo i j) ;    enumFromThenTo (CTcflag i) (CTcflag j) (CTcflag k) = map CTcflag (enumFromThenTo i j k) } ; instance Storable CTcflag where {    sizeOf    (CTcflag x)       = sizeOf x ;    alignment (CTcflag x)       = alignment x ;    peekElemOff a i       = liftM CTcflag (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CTcflag x) = pokeElemOff (castPtr a) i x } ; tyConCTcflag = mkTyCon "CTcflag"; instance Typeable CTcflag where { typeOf _ = mkAppTy tyConCTcflag [] } ; ; instance Bounded CTcflag where {    minBound = CTcflag minBound ;    maxBound = CTcflag maxBound } ; instance Real CTcflag where {    toRational (CTcflag i) = toRational i } ; instance Integral CTcflag where {    (CTcflag i) `quot`    (CTcflag j) = CTcflag (i `quot` j) ;    (CTcflag i) `rem`     (CTcflag j) = CTcflag (i `rem`  j) ;    (CTcflag i) `div`     (CTcflag j) = CTcflag (i `div`  j) ;    (CTcflag i) `mod`     (CTcflag j) = CTcflag (i `mod`  j) ;    (CTcflag i) `quotRem` (CTcflag j) = let (q,r) = i `quotRem` j in (CTcflag q, CTcflag r) ;    (CTcflag i) `divMod`  (CTcflag j) = let (d,m) = i `divMod`  j in (CTcflag d, CTcflag m) ;    toInteger (CTcflag i)       = toInteger i } ; instance Bits CTcflag where {   (CTcflag x) .&.     (CTcflag y)   = CTcflag (x .&.   y) ;   (CTcflag x) .|.     (CTcflag y)   = CTcflag (x .|.   y) ;   (CTcflag x) `xor`   (CTcflag y)   = CTcflag (x `xor` y) ;   complement    (CTcflag x)   = CTcflag (complement x) ;   shift         (CTcflag x) n = CTcflag (shift x n) ;   rotate        (CTcflag x) n = CTcflag (rotate x n) ;   bit                 n = CTcflag (bit n) ;   setBit        (CTcflag x) n = CTcflag (setBit x n) ;   clearBit      (CTcflag x) n = CTcflag (clearBit x n) ;   complementBit (CTcflag x) n = CTcflag (complementBit x n) ;   testBit       (CTcflag x) n = testBit x n ;   bitSize       (CTcflag x)   = bitSize x ;   isSigned      (CTcflag x)   = isSigned x }

newtype CRLim = CRLim Word32 deriving (Eq, Ord) ; instance Num CRLim where {    (CRLim i) + (CRLim j) = CRLim (i + j) ;    (CRLim i) - (CRLim j) = CRLim (i - j) ;    (CRLim i) * (CRLim j) = CRLim (i * j) ;    negate  (CRLim i) = CRLim (negate i) ;    abs     (CRLim i) = CRLim (abs    i) ;    signum  (CRLim i) = CRLim (signum i) ;    fromInteger x = CRLim (fromInteger x) } ; instance Read CRLim where {    readsPrec p s = map (\(x, t) -> (CRLim x, t)) (readsPrec p s) } ; instance Show CRLim where {    showsPrec p (CRLim x) = showsPrec p x } ; instance Enum CRLim where {    succ           (CRLim i)             = CRLim (succ i) ;    pred           (CRLim i)             = CRLim (pred i) ;    toEnum               x           = CRLim (toEnum x) ;    fromEnum       (CRLim i)             = fromEnum i ;    enumFrom       (CRLim i)             = map CRLim (enumFrom i) ;    enumFromThen   (CRLim i) (CRLim j)       = map CRLim (enumFromThen i j) ;    enumFromTo     (CRLim i) (CRLim j)       = map CRLim (enumFromTo i j) ;    enumFromThenTo (CRLim i) (CRLim j) (CRLim k) = map CRLim (enumFromThenTo i j k) } ; instance Storable CRLim where {    sizeOf    (CRLim x)       = sizeOf x ;    alignment (CRLim x)       = alignment x ;    peekElemOff a i       = liftM CRLim (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CRLim x) = pokeElemOff (castPtr a) i x } ; tyConCRlim = mkTyCon "CRLim"; instance Typeable CRLim where { typeOf _ = mkAppTy tyConCRlim [] } ; ; instance Bounded CRLim where {    minBound = CRLim minBound ;    maxBound = CRLim maxBound } ; instance Real CRLim where {    toRational (CRLim i) = toRational i } ; instance Integral CRLim where {    (CRLim i) `quot`    (CRLim j) = CRLim (i `quot` j) ;    (CRLim i) `rem`     (CRLim j) = CRLim (i `rem`  j) ;    (CRLim i) `div`     (CRLim j) = CRLim (i `div`  j) ;    (CRLim i) `mod`     (CRLim j) = CRLim (i `mod`  j) ;    (CRLim i) `quotRem` (CRLim j) = let (q,r) = i `quotRem` j in (CRLim q, CRLim r) ;    (CRLim i) `divMod`  (CRLim j) = let (d,m) = i `divMod`  j in (CRLim d, CRLim m) ;    toInteger (CRLim i)       = toInteger i } ; instance Bits CRLim where {   (CRLim x) .&.     (CRLim y)   = CRLim (x .&.   y) ;   (CRLim x) .|.     (CRLim y)   = CRLim (x .|.   y) ;   (CRLim x) `xor`   (CRLim y)   = CRLim (x `xor` y) ;   complement    (CRLim x)   = CRLim (complement x) ;   shift         (CRLim x) n = CRLim (shift x n) ;   rotate        (CRLim x) n = CRLim (rotate x n) ;   bit                 n = CRLim (bit n) ;   setBit        (CRLim x) n = CRLim (setBit x n) ;   clearBit      (CRLim x) n = CRLim (clearBit x n) ;   complementBit (CRLim x) n = CRLim (complementBit x n) ;   testBit       (CRLim x) n = testBit x n ;   bitSize       (CRLim x)   = bitSize x ;   isSigned      (CRLim x)   = isSigned x }

-- ToDo: blksize_t, clockid_t, blkcnt_t, fsblkcnt_t, fsfilcnt_t, id_t, key_t
-- suseconds_t, timer_t, useconds_t

-- Make an Fd type rather than using CInt everywhere
newtype Fd = Fd CInt deriving (Eq, Ord) ; instance Num Fd where {    (Fd i) + (Fd j) = Fd (i + j) ;    (Fd i) - (Fd j) = Fd (i - j) ;    (Fd i) * (Fd j) = Fd (i * j) ;    negate  (Fd i) = Fd (negate i) ;    abs     (Fd i) = Fd (abs    i) ;    signum  (Fd i) = Fd (signum i) ;    fromInteger x = Fd (fromInteger x) } ; instance Read Fd where {    readsPrec p s = map (\(x, t) -> (Fd x, t)) (readsPrec p s) } ; instance Show Fd where {    showsPrec p (Fd x) = showsPrec p x } ; instance Enum Fd where {    succ           (Fd i)             = Fd (succ i) ;    pred           (Fd i)             = Fd (pred i) ;    toEnum               x           = Fd (toEnum x) ;    fromEnum       (Fd i)             = fromEnum i ;    enumFrom       (Fd i)             = map Fd (enumFrom i) ;    enumFromThen   (Fd i) (Fd j)       = map Fd (enumFromThen i j) ;    enumFromTo     (Fd i) (Fd j)       = map Fd (enumFromTo i j) ;    enumFromThenTo (Fd i) (Fd j) (Fd k) = map Fd (enumFromThenTo i j k) } ; instance Storable Fd where {    sizeOf    (Fd x)       = sizeOf x ;    alignment (Fd x)       = alignment x ;    peekElemOff a i       = liftM Fd (peekElemOff (castPtr a) i) ;    pokeElemOff a i (Fd x) = pokeElemOff (castPtr a) i x } ; tyConFd = mkTyCon "Fd"; instance Typeable Fd where { typeOf _ = mkAppTy tyConFd [] } ; ; instance Bounded Fd where {    minBound = Fd minBound ;    maxBound = Fd maxBound } ; instance Real Fd where {    toRational (Fd i) = toRational i } ; instance Integral Fd where {    (Fd i) `quot`    (Fd j) = Fd (i `quot` j) ;    (Fd i) `rem`     (Fd j) = Fd (i `rem`  j) ;    (Fd i) `div`     (Fd j) = Fd (i `div`  j) ;    (Fd i) `mod`     (Fd j) = Fd (i `mod`  j) ;    (Fd i) `quotRem` (Fd j) = let (q,r) = i `quotRem` j in (Fd q, Fd r) ;    (Fd i) `divMod`  (Fd j) = let (d,m) = i `divMod`  j in (Fd d, Fd m) ;    toInteger (Fd i)       = toInteger i } ; instance Bits Fd where {   (Fd x) .&.     (Fd y)   = Fd (x .&.   y) ;   (Fd x) .|.     (Fd y)   = Fd (x .|.   y) ;   (Fd x) `xor`   (Fd y)   = Fd (x `xor` y) ;   complement    (Fd x)   = Fd (complement x) ;   shift         (Fd x) n = Fd (shift x n) ;   rotate        (Fd x) n = Fd (rotate x n) ;   bit                 n = Fd (bit n) ;   setBit        (Fd x) n = Fd (setBit x n) ;   clearBit      (Fd x) n = Fd (clearBit x n) ;   complementBit (Fd x) n = Fd (complementBit x n) ;   testBit       (Fd x) n = testBit x n ;   bitSize       (Fd x)   = bitSize x ;   isSigned      (Fd x)   = isSigned x }

-- nicer names, and backwards compatibility with POSIX library:

type LinkCount      = CNlink

type UserID         = CUid

type GroupID        = CGid

type ByteCount      = CSize
type ClockTick      = CClock
type EpochTime      = CTime
type DeviceID       = CDev
type FileID         = CIno
type FileMode       = CMode
type ProcessID      = CPid
type FileOffset     = COff
type ProcessGroupID = CPid
type Limit	    = CLong

