{-# LINE 1 "Prim.hsc" #-}
{-# LINE 2 "Prim.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.DynamicLinker.Prim
-- Copyright   :  (c) Volker Stolz <vs@foldr.org> 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  vs@foldr.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- DLOpen and friend
--  Derived from GModule.chs by M.Weber & M.Chakravarty which is part of c2hs
--  I left the API more or less the same, mostly the flags are different.
--
-----------------------------------------------------------------------------

module System.Posix.DynamicLinker.Prim (
  -- * low level API
  c_dlopen,
  c_dlsym,
  c_dlerror,
  c_dlclose,
  -- dlAddr, -- XXX NYI
  haveRtldNext,
  haveRtldLocal,
  packRTLDFlags,
  RTLDFlags(..),
  packDL,
  DL(..)
 )

where


{-# LINE 36 "Prim.hsc" #-}

import Data.Bits	( (.|.) )
import Foreign.Ptr	( Ptr, FunPtr, nullPtr )
import Foreign.C.Types	( CInt )
import Foreign.C.String	( CString )

-- RTLD_NEXT madness
-- On some host (e.g. SuSe Linux 7.2) RTLD_NEXT is not visible
-- without setting _GNU_SOURCE. Since we don't want to set this
-- flag, here's a different solution: You can use the Haskell
-- function 'haveRtldNext' to check wether the flag is available
-- to you. Ideally, this will be optimized by the compiler so
-- that it should be as efficient as an #ifdef.
--    If you fail to test the flag and use it although it is
-- undefined, 'packOneModuleFlag' will bomb.
--    The same applies to RTLD_LOCAL which isn't available on
-- cygwin.

haveRtldNext :: Bool


{-# LINE 62 "Prim.hsc" #-}
haveRtldNext = False

{-# LINE 64 "Prim.hsc" #-}

haveRtldLocal :: Bool


{-# LINE 70 "Prim.hsc" #-}
haveRtldLocal = False

{-# LINE 72 "Prim.hsc" #-}

data RTLDFlags 
  = RTLD_LAZY
  | RTLD_NOW
  | RTLD_GLOBAL 
  | RTLD_LOCAL
    deriving (Show, Read)

foreign import ccall unsafe "HsUnix.h dlopen" c_dlopen :: CString -> CInt -> IO (Ptr ())
foreign import ccall unsafe "HsUnix.h dlsym"  c_dlsym  :: Ptr () -> CString -> IO (FunPtr a)
foreign import ccall unsafe "HsUnix.h dlerror" c_dlerror :: IO CString
foreign import ccall unsafe "HsUnix.h dlclose" c_dlclose :: (Ptr ()) -> IO CInt

packRTLDFlags :: [RTLDFlags] -> CInt
packRTLDFlags flags = foldl (\ s f -> (packRTLDFlag f) .|. s) 0 flags

packRTLDFlag :: RTLDFlags -> CInt
packRTLDFlag RTLD_LAZY = 1
{-# LINE 90 "Prim.hsc" #-}


{-# LINE 94 "Prim.hsc" #-}
packRTLDFlag RTLD_NOW =  error "RTLD_NOW not available"

{-# LINE 96 "Prim.hsc" #-}


{-# LINE 100 "Prim.hsc" #-}
packRTLDFlag RTLD_GLOBAL = error "RTLD_GLOBAL not available"

{-# LINE 102 "Prim.hsc" #-}


{-# LINE 106 "Prim.hsc" #-}
packRTLDFlag RTLD_LOCAL = error "RTLD_LOCAL not available"

{-# LINE 108 "Prim.hsc" #-}

-- |Flags for 'dlsym'. Notice that @Next@ might not be available on
-- your particular platform!

data DL = Null | Next | Default | DLHandle (Ptr ()) deriving (Show)

packDL :: DL -> Ptr ()
packDL Null = nullPtr

{-# LINE 119 "Prim.hsc" #-}
packDL Next = error "RTLD_NEXT not available"

{-# LINE 121 "Prim.hsc" #-}
packDL Default = nullPtr
packDL (DLHandle h) = h
