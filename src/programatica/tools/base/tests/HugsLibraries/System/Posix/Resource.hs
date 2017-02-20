{-# LINE 1 "Resource.hsc" #-}
{-# LINE 2 "Resource.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Resource
-- Copyright   :  (c) The University of Glasgow 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX resource support
--
-----------------------------------------------------------------------------

module System.Posix.Resource (
    -- * Resource Limits
    ResourceLimit(..), ResourceLimits(..), Resource(..),
    getResourceLimit,
    setResourceLimit,
  ) where


{-# LINE 24 "Resource.hsc" #-}

import System.Posix.Types
import Foreign
import Foreign.C

-- -----------------------------------------------------------------------------
-- Resource limits

data Resource
  = ResourceCoreFileSize
  | ResourceCPUTime
  | ResourceDataSize
  | ResourceFileSize
  | ResourceOpenFiles
  | ResourceStackSize

{-# LINE 40 "Resource.hsc" #-}
  | ResourceTotalMemory

{-# LINE 42 "Resource.hsc" #-}
  deriving Eq

data ResourceLimits
  = ResourceLimits { softLimit, hardLimit :: ResourceLimit }
  deriving Eq

data ResourceLimit
  = ResourceLimitInfinity
  | ResourceLimitUnknown
  | ResourceLimit Integer
  deriving Eq

type RLimit = ()

foreign import ccall unsafe "HsUnix.h getrlimit"
  c_getrlimit :: CInt -> Ptr RLimit -> IO CInt

foreign import ccall unsafe "HsUnix.h setrlimit"
  c_setrlimit :: CInt -> Ptr RLimit -> IO CInt

getResourceLimit :: Resource -> IO ResourceLimits
getResourceLimit res = do
  allocaBytes (8) $ \p_rlimit -> do
{-# LINE 65 "Resource.hsc" #-}
    throwErrnoIfMinus1 "getResourceLimit" $
      c_getrlimit (packResource res) p_rlimit
    soft <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p_rlimit
{-# LINE 68 "Resource.hsc" #-}
    hard <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p_rlimit
{-# LINE 69 "Resource.hsc" #-}
    return (ResourceLimits { 
		softLimit = unpackRLimit soft,
		hardLimit = unpackRLimit hard
	   })

setResourceLimit :: Resource -> ResourceLimits -> IO ()
setResourceLimit res ResourceLimits{softLimit=soft,hardLimit=hard} = do
  allocaBytes (8) $ \p_rlimit -> do
{-# LINE 77 "Resource.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p_rlimit (packRLimit soft True)
{-# LINE 78 "Resource.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p_rlimit (packRLimit hard False)
{-# LINE 79 "Resource.hsc" #-}
    throwErrnoIfMinus1 "setResourceLimit" $
	c_setrlimit (packResource res) p_rlimit
    return ()

packResource :: Resource -> CInt
packResource ResourceCoreFileSize  = (4)
{-# LINE 85 "Resource.hsc" #-}
packResource ResourceCPUTime       = (0)
{-# LINE 86 "Resource.hsc" #-}
packResource ResourceDataSize      = (2)
{-# LINE 87 "Resource.hsc" #-}
packResource ResourceFileSize      = (1)
{-# LINE 88 "Resource.hsc" #-}
packResource ResourceOpenFiles     = (7)
{-# LINE 89 "Resource.hsc" #-}
packResource ResourceStackSize     = (3)
{-# LINE 90 "Resource.hsc" #-}

{-# LINE 91 "Resource.hsc" #-}
packResource ResourceTotalMemory   = (9)
{-# LINE 92 "Resource.hsc" #-}

{-# LINE 93 "Resource.hsc" #-}

unpackRLimit :: CRLim -> ResourceLimit
unpackRLimit (4294967295)  = ResourceLimitInfinity
{-# LINE 96 "Resource.hsc" #-}

{-# LINE 97 "Resource.hsc" #-}
unpackRLimit (4294967295) = ResourceLimitUnknown
{-# LINE 98 "Resource.hsc" #-}
unpackRLimit (4294967295) = ResourceLimitUnknown
{-# LINE 99 "Resource.hsc" #-}

{-# LINE 100 "Resource.hsc" #-}
unpackRLimit other = ResourceLimit (fromIntegral other)

packRLimit :: ResourceLimit -> Bool -> CRLim
packRLimit ResourceLimitInfinity _     = (4294967295)
{-# LINE 104 "Resource.hsc" #-}

{-# LINE 105 "Resource.hsc" #-}
packRLimit ResourceLimitUnknown  True  = (4294967295)
{-# LINE 106 "Resource.hsc" #-}
packRLimit ResourceLimitUnknown  False = (4294967295)
{-# LINE 107 "Resource.hsc" #-}

{-# LINE 108 "Resource.hsc" #-}
packRLimit (ResourceLimit other) _     = fromIntegral other


-- -----------------------------------------------------------------------------
-- Test code

{-
import System.Posix
import Control.Monad

main = do
 zipWithM_ (\r n -> setResourceLimit r ResourceLimits{
					hardLimit = ResourceLimit n,
					softLimit = ResourceLimit n })
	allResources [1..]	
 showAll
 mapM_ (\r -> setResourceLimit r ResourceLimits{
					hardLimit = ResourceLimit 1,
					softLimit = ResourceLimitInfinity })
	allResources
   -- should fail


showAll = 
  mapM_ (\r -> getResourceLimit r >>= (putStrLn . showRLims)) allResources

allResources =
    [ResourceCoreFileSize, ResourceCPUTime, ResourceDataSize,
	ResourceFileSize, ResourceOpenFiles, ResourceStackSize
#ifdef RLIMIT_AS
	, ResourceTotalMemory 
#endif
	]

showRLims ResourceLimits{hardLimit=h,softLimit=s}
  = "hard: " ++ showRLim h ++ ", soft: " ++ showRLim s
 
showRLim ResourceLimitInfinity = "infinity"
showRLim ResourceLimitUnknown  = "unknown"
showRLim (ResourceLimit other)  = show other
-}
