{-# LINE 1 "Process.hsc" #-}
{-# LINE 2 "Process.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Process
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX process support
--
-----------------------------------------------------------------------------

module System.Posix.Process (
    -- * Processes

    -- ** Forking and executing

{-# LINE 23 "Process.hsc" #-}
    executeFile,
    
    -- ** Exiting
    exitImmediately,

    -- ** Process environment
    getProcessID,
    getParentProcessID,
    getProcessGroupID,

    -- ** Process groups
    createProcessGroup,
    joinProcessGroup,
    setProcessGroupID,

    -- ** Sessions
    createSession,

    -- ** Process times
    ProcessTimes(..),
    getProcessTimes,

    -- ** Scheduling priority
    nice,
    getProcessPriority,
    getProcessGroupPriority,
    getUserPriority,
    setProcessPriority,
    setProcessGroupPriority,
    setUserPriority,

    -- ** Process status
    ProcessStatus(..),
    getProcessStatus,
    getAnyProcessStatus,
    getGroupProcessStatus,

 ) where


{-# LINE 63 "Process.hsc" #-}

import Foreign.C.Error
import Foreign.C.String ( CString, withCString )
import Foreign.C.Types ( CInt, CClock )
import Foreign.Marshal.Alloc ( alloca, allocaBytes )
import Foreign.Marshal.Array ( withArray0 )
import Foreign.Marshal.Utils ( withMany )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.StablePtr ( StablePtr, newStablePtr, freeStablePtr )
import Foreign.Storable ( Storable(..) )
import System.IO
import System.IO.Error
import System.Exit
import System.Posix.Types
import System.Posix.Signals
import Control.Monad


{-# LINE 81 "Process.hsc" #-}
{-# CBITS HsUnix.c execvpe.c #-}

{-# LINE 83 "Process.hsc" #-}

-- -----------------------------------------------------------------------------
-- Process environment

getProcessID :: IO ProcessID
getProcessID = c_getpid

foreign import ccall unsafe "HsUnix.h getpid"
   c_getpid :: IO CPid

getParentProcessID :: IO ProcessID
getParentProcessID = c_getppid

foreign import ccall unsafe "HsUnix.h getppid"
  c_getppid :: IO CPid

getProcessGroupID :: IO ProcessGroupID
getProcessGroupID = c_getpgrp

foreign import ccall unsafe "HsUnix.h getpgrp"
  c_getpgrp :: IO CPid

createProcessGroup :: ProcessID -> IO ProcessGroupID
createProcessGroup pid = do
  throwErrnoIfMinus1_ "createProcessGroup" (c_setpgid pid 0)
  return pid

joinProcessGroup :: ProcessGroupID -> IO ()
joinProcessGroup pgid =
  throwErrnoIfMinus1_ "joinProcessGroup" (c_setpgid 0 pgid)

setProcessGroupID :: ProcessID -> ProcessGroupID -> IO ()
setProcessGroupID pid pgid =
  throwErrnoIfMinus1_ "setProcessGroupID" (c_setpgid pid pgid)

foreign import ccall unsafe "HsUnix.h setpgid"
  c_setpgid :: CPid -> CPid -> IO CInt

createSession :: IO ProcessGroupID
createSession = throwErrnoIfMinus1 "createSession" c_setsid

foreign import ccall unsafe "HsUnix.h setsid"
  c_setsid :: IO CPid

-- -----------------------------------------------------------------------------
-- Process times

-- All times in clock ticks (see getClockTick)

data ProcessTimes
  = ProcessTimes { elapsedTime     :: ClockTick
  		 , userTime        :: ClockTick
		 , systemTime      :: ClockTick
		 , childUserTime   :: ClockTick
		 , childSystemTime :: ClockTick
		 }

getProcessTimes :: IO ProcessTimes
getProcessTimes = do
   allocaBytes (16) $ \p_tms -> do
{-# LINE 143 "Process.hsc" #-}
     elapsed <- throwErrnoIfMinus1 "getProcessTimes" (c_times p_tms)
     ut  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  p_tms
{-# LINE 145 "Process.hsc" #-}
     st  <- ((\hsc_ptr -> peekByteOff hsc_ptr 4))  p_tms
{-# LINE 146 "Process.hsc" #-}
     cut <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p_tms
{-# LINE 147 "Process.hsc" #-}
     cst <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) p_tms
{-# LINE 148 "Process.hsc" #-}
     return (ProcessTimes{ elapsedTime     = elapsed,
	 		   userTime        = ut,
	 		   systemTime      = st,
	 		   childUserTime   = cut,
	 		   childSystemTime = cst
			  })

type CTms = ()

foreign import ccall unsafe "HsUnix.h times"
  c_times :: Ptr CTms -> IO CClock

-- -----------------------------------------------------------------------------
-- Process scheduling priority

nice :: Int -> IO ()
nice prio = do
  resetErrno
  res <- c_nice (fromIntegral prio)
  when (res == -1) $ do
    err <- getErrno
    when (err /= eOK) (throwErrno "nice")

foreign import ccall unsafe "HsUnix.h nice"
  c_nice :: CInt -> IO CInt

getProcessPriority      :: ProcessID      -> IO Int
getProcessGroupPriority :: ProcessGroupID -> IO Int
getUserPriority         :: UserID         -> IO Int

getProcessPriority pid = do
  r <- throwErrnoIfMinus1 "getProcessPriority" $
         c_getpriority (0) (fromIntegral pid)
{-# LINE 181 "Process.hsc" #-}
  return (fromIntegral r)

getProcessGroupPriority pid = do
  r <- throwErrnoIfMinus1 "getProcessPriority" $
         c_getpriority (1) (fromIntegral pid)
{-# LINE 186 "Process.hsc" #-}
  return (fromIntegral r)

getUserPriority uid = do
  r <- throwErrnoIfMinus1 "getUserPriority" $
         c_getpriority (2) (fromIntegral uid)
{-# LINE 191 "Process.hsc" #-}
  return (fromIntegral r)

foreign import ccall unsafe "HsUnix.h getpriority"
  c_getpriority :: CInt -> CInt -> IO CInt

setProcessPriority      :: ProcessID      -> Int -> IO ()
setProcessGroupPriority :: ProcessGroupID -> Int -> IO ()
setUserPriority         :: UserID         -> Int -> IO ()

setProcessPriority pid val = 
  throwErrnoIfMinus1_ "setProcessPriority" $
    c_setpriority (0) (fromIntegral pid) (fromIntegral val)
{-# LINE 203 "Process.hsc" #-}

setProcessGroupPriority pid val =
  throwErrnoIfMinus1_ "setProcessPriority" $
    c_setpriority (1) (fromIntegral pid) (fromIntegral val)
{-# LINE 207 "Process.hsc" #-}

setUserPriority uid val =
  throwErrnoIfMinus1_ "setUserPriority" $
    c_setpriority (2) (fromIntegral uid) (fromIntegral val)
{-# LINE 211 "Process.hsc" #-}

foreign import ccall unsafe "HsUnix.h setpriority"
  c_setpriority :: CInt -> CInt -> CInt -> IO CInt

-- -----------------------------------------------------------------------------
-- Forking, execution


{-# LINE 235 "Process.hsc" #-}

executeFile :: FilePath			    -- Command
            -> Bool			    -- Search PATH?
            -> [String]			    -- Arguments
            -> Maybe [(String, String)]	    -- Environment
            -> IO ()
executeFile path search args Nothing = do
  withCString path $ \s ->
    withMany withCString (path:args) $ \cstrs ->
      withArray0 nullPtr cstrs $ \arr -> do
	pPrPr_disableITimers
	if search 
	   then throwErrnoIfMinus1_ "executeFile" (c_execvp s arr)
	   else throwErrnoIfMinus1_ "executeFile" (c_execv s arr)

executeFile path search args (Just env) = do
  withCString path $ \s ->
    withMany withCString (path:args) $ \cstrs ->
      withArray0 nullPtr cstrs $ \arg_arr ->
    let env' = map (\ (name, val) -> name ++ ('=' : val)) env in
    withMany withCString env' $ \cenv ->
      withArray0 nullPtr cenv $ \env_arr -> do
	pPrPr_disableITimers
	if search 
	   then throwErrnoIfMinus1_ "executeFile" (c_execvpe s arg_arr env_arr)
	   else throwErrnoIfMinus1_ "executeFile" (c_execve s arg_arr env_arr)

-- this function disables the itimer, which would otherwise cause confusing
-- signals to be sent to the new process.
foreign import ccall unsafe "HsUnix.h pPrPr_disableITimers"
  pPrPr_disableITimers :: IO ()

foreign import ccall unsafe "HsUnix.h execvp"
  c_execvp :: CString -> Ptr CString -> IO CInt

foreign import ccall unsafe "HsUnix.h execv"
  c_execv :: CString -> Ptr CString -> IO CInt

foreign import ccall unsafe "HsUnix.h execvpe"
  c_execvpe :: CString -> Ptr CString -> Ptr CString -> IO CInt

foreign import ccall unsafe "HsUnix.h execve"
  c_execve :: CString -> Ptr CString -> Ptr CString -> IO CInt

-- -----------------------------------------------------------------------------
-- Waiting for process termination

data ProcessStatus = Exited ExitCode
                   | Terminated Signal
                   | Stopped Signal
		   deriving (Eq, Ord, Show)

getProcessStatus :: Bool -> Bool -> ProcessID -> IO (Maybe ProcessStatus)
getProcessStatus block stopped pid =
  alloca $ \wstatp -> do
    pid <- throwErrnoIfMinus1Retry "getProcessStatus"
		(c_waitpid pid wstatp (waitOptions block stopped))
    case pid of
      0  -> return Nothing
      _  -> do ps <- decipherWaitStatus wstatp
	       return (Just ps)

foreign import ccall unsafe "HsUnix.h waitpid"
  c_waitpid :: CPid -> Ptr CInt -> CInt -> IO CPid

getGroupProcessStatus :: Bool
                      -> Bool
                      -> ProcessGroupID
                      -> IO (Maybe (ProcessID, ProcessStatus))
getGroupProcessStatus block stopped pgid =
  alloca $ \wstatp -> do
    pid <- throwErrnoIfMinus1Retry "getGroupProcessStatus"
		(c_waitpid (-pgid) wstatp (waitOptions block stopped))
    case pid of
      0  -> return Nothing
      _  -> do ps <- decipherWaitStatus wstatp
	       return (Just (pid, ps))

getAnyProcessStatus :: Bool -> Bool -> IO (Maybe (ProcessID, ProcessStatus))
getAnyProcessStatus block stopped = getGroupProcessStatus block stopped 1

waitOptions :: Bool -> Bool -> CInt
--             block   stopped
waitOptions False False = (1)
{-# LINE 319 "Process.hsc" #-}
waitOptions False True  = (3)
{-# LINE 320 "Process.hsc" #-}
waitOptions True  False = 0
waitOptions True  True  = (2)
{-# LINE 322 "Process.hsc" #-}

-- Turn a (ptr to a) wait status into a ProcessStatus

decipherWaitStatus :: Ptr CInt -> IO ProcessStatus
decipherWaitStatus wstatp = do
  wstat <- peek wstatp
  if c_WIFEXITED wstat /= 0
      then do
        let exitstatus = c_WEXITSTATUS wstat
        if exitstatus == 0
	   then return (Exited ExitSuccess)
	   else return (Exited (ExitFailure (fromIntegral exitstatus)))
      else do
        if c_WIFSIGNALED wstat /= 0
	   then do
		let termsig = c_WTERMSIG wstat
		return (Terminated (fromIntegral termsig))
	   else do
		if c_WIFSTOPPED wstat /= 0
		   then do
			let stopsig = c_WSTOPSIG wstat
			return (Stopped (fromIntegral stopsig))
		   else do
			ioError (mkIOError illegalOperationErrorType
				   "waitStatus" Nothing Nothing)

foreign import ccall unsafe "HsUnix.h __hsunix_wifexited"
  c_WIFEXITED :: CInt -> CInt 

foreign import ccall unsafe "HsUnix.h __hsunix_wexitstatus"
  c_WEXITSTATUS :: CInt -> CInt

foreign import ccall unsafe "HsUnix.h __hsunix_wifsignaled"
  c_WIFSIGNALED :: CInt -> CInt

foreign import ccall unsafe "HsUnix.h __hsunix_wtermsig"
  c_WTERMSIG :: CInt -> CInt 

foreign import ccall unsafe "HsUnix.h __hsunix_wifstopped"
  c_WIFSTOPPED :: CInt -> CInt

foreign import ccall unsafe "HsUnix.h __hsunix_wstopsig"
  c_WSTOPSIG :: CInt -> CInt

-- -----------------------------------------------------------------------------
-- Exiting

exitImmediately :: ExitCode -> IO ()
exitImmediately exitcode = c_exit (exitcode2Int exitcode)
  where
    exitcode2Int ExitSuccess = 0
    exitcode2Int (ExitFailure n) = fromIntegral n

foreign import ccall unsafe "HsUnix.h exit"
  c_exit :: CInt -> IO ()

-- -----------------------------------------------------------------------------
