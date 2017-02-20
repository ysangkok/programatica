{-# LINE 1 "Unistd.hsc" #-}
{-# LINE 2 "Unistd.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Unistd
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX miscellaneous stuff, mostly from unistd.h
--
-----------------------------------------------------------------------------

module System.Posix.Unistd (
    -- * System environment
    SystemID(..),
    getSystemID,

    SysVar(..),
    getSysVar,

    -- * Sleeping
    sleep, usleep,

  {-
    ToDo from unistd.h:
      confstr, 
      lots of sysconf variables

    -- use Network.BSD
    gethostid, gethostname

    -- should be in System.Posix.Files?
    pathconf, fpathconf,

    -- System.Posix.Signals
    ualarm,

    -- System.Posix.IO
    read, write,

    -- should be in System.Posix.User?
    getEffectiveUserName,
-}
  ) where


{-# LINE 50 "Unistd.hsc" #-}

import Foreign.C.Error ( throwErrnoIfMinus1, throwErrnoIfMinus1_ )
import Foreign.C.String ( peekCString )
import Foreign.C.Types ( CInt, CUInt, CLong )
import Foreign.Marshal.Alloc ( allocaBytes )
import Foreign.Ptr ( Ptr, plusPtr )
import System.Posix.Types
import System.Posix.Internals

-- -----------------------------------------------------------------------------
-- System environment (uname())

data SystemID =
  SystemID { systemName :: String
  	   , nodeName   :: String
	   , release    :: String
	   , version    :: String
	   , machine    :: String
	   }

getSystemID :: IO SystemID
getSystemID = do
  allocaBytes (390) $ \p_sid -> do
{-# LINE 73 "Unistd.hsc" #-}
    throwErrnoIfMinus1_ "getSystemID" (c_uname p_sid)
    sysN <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 0)) p_sid)
{-# LINE 75 "Unistd.hsc" #-}
    node <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 65)) p_sid)
{-# LINE 76 "Unistd.hsc" #-}
    rel  <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 130)) p_sid)
{-# LINE 77 "Unistd.hsc" #-}
    ver  <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 195)) p_sid)
{-# LINE 78 "Unistd.hsc" #-}
    mach <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 260)) p_sid)
{-# LINE 79 "Unistd.hsc" #-}
    return (SystemID { systemName = sysN,
		       nodeName   = node,
		       release    = rel,
		       version    = ver,
		       machine    = mach
		     })

foreign import ccall unsafe "HsUnix.h uname"
   c_uname :: Ptr CUtsname -> IO CInt

-- -----------------------------------------------------------------------------
-- sleeping

sleep :: Int -> IO Int
sleep 0 = return 0
sleep secs = do r <- c_sleep (fromIntegral secs); return (fromIntegral r)

foreign import ccall unsafe "HsUnix.h sleep"
  c_sleep :: CUInt -> IO CUInt

usleep :: Int -> IO ()
usleep 0 = return ()

{-# LINE 104 "Unistd.hsc" #-}
usleep usecs = throwErrnoIfMinus1_ "usleep" (c_usleep (fromIntegral usecs))

{-# LINE 106 "Unistd.hsc" #-}


{-# LINE 111 "Unistd.hsc" #-}
foreign import ccall unsafe "HsUnix.h usleep"
  c_usleep :: CUInt -> IO CInt

{-# LINE 114 "Unistd.hsc" #-}

-- -----------------------------------------------------------------------------
-- System variables

data SysVar = ArgumentLimit
            | ChildLimit
            | ClockTick
            | GroupLimit
            | OpenFileLimit
            | PosixVersion
            | HasSavedIDs
            | HasJobControl
	-- ToDo: lots more

getSysVar :: SysVar -> IO Integer
getSysVar v =
    case v of
      ArgumentLimit -> sysconf (0)
{-# LINE 132 "Unistd.hsc" #-}
      ChildLimit    -> sysconf (1)
{-# LINE 133 "Unistd.hsc" #-}
      ClockTick	    -> sysconf (2)
{-# LINE 134 "Unistd.hsc" #-}
      GroupLimit    -> sysconf (3)
{-# LINE 135 "Unistd.hsc" #-}
      OpenFileLimit -> sysconf (4)
{-# LINE 136 "Unistd.hsc" #-}
      PosixVersion  -> sysconf (29)
{-# LINE 137 "Unistd.hsc" #-}
      HasSavedIDs   -> sysconf (8)
{-# LINE 138 "Unistd.hsc" #-}
      HasJobControl -> sysconf (7)
{-# LINE 139 "Unistd.hsc" #-}

sysconf :: CInt -> IO Integer
sysconf n = do 
  r <- throwErrnoIfMinus1 "getSysVar" (c_sysconf n)
  return (fromIntegral r)

foreign import ccall unsafe "HsUnix.h sysconf"
  c_sysconf :: CInt -> IO CLong
