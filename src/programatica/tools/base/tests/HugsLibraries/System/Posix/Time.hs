{-# LINE 1 "Time.hsc" #-}
{-# LINE 2 "Time.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Time
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX Time support
--
-----------------------------------------------------------------------------

module System.Posix.Time (
	epochTime,
	-- ToDo: lots more from sys/time.h
	-- how much already supported by System.Time?
  ) where


{-# LINE 23 "Time.hsc" #-}

import System.Posix.Types
import Foreign
import Foreign.C

-- -----------------------------------------------------------------------------
-- epochTime

epochTime :: IO EpochTime
epochTime = throwErrnoIfMinus1 "epochTime" (c_time nullPtr)

foreign import ccall unsafe "HsUnix.h time"
  c_time :: Ptr CTime -> IO CTime
