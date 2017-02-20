{-# LINE 1 "Signals.hsc" #-}
-----------------------------------------------------------------------------
{-# LINE 2 "Signals.hsc" #-}
-- |
-- Module      :  System.Posix.Signals
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX signal support
--
-----------------------------------------------------------------------------


{-# LINE 16 "Signals.hsc" #-}

module System.Posix.Signals (

{-# LINE 19 "Signals.hsc" #-}
  -- * The Signal type
  Signal,

  -- * Specific signals
  nullSignal,
  internalAbort, sigABRT,
  realTimeAlarm, sigALRM,
  busError, sigBUS,
  processStatusChanged, sigCHLD,
  continueProcess, sigCONT,
  floatingPointException, sigFPE,
  lostConnection, sigHUP,
  illegalInstruction, sigILL,
  keyboardSignal, sigINT,
  killProcess, sigKILL,
  openEndedPipe, sigPIPE,
  keyboardTermination, sigQUIT,
  segmentationViolation, sigSEGV,
  softwareStop, sigSTOP,
  softwareTermination, sigTERM,
  keyboardStop, sigTSTP,
  backgroundRead, sigTTIN,
  backgroundWrite, sigTTOU,
  userDefinedSignal1, sigUSR1,
  userDefinedSignal2, sigUSR2,

{-# LINE 45 "Signals.hsc" #-}
  pollableEvent, sigPOLL,

{-# LINE 47 "Signals.hsc" #-}
  profilingTimerExpired, sigPROF,
  badSystemCall, sigSYS,
  breakpointTrap, sigTRAP,
  urgentDataAvailable, sigURG,
  virtualTimerExpired, sigVTALRM,
  cpuTimeLimitExceeded, sigXCPU,
  fileSizeLimitExceeded, sigXFSZ,

  -- * Sending signals
  raiseSignal,
  signalProcess,
  signalProcessGroup,


{-# LINE 65 "Signals.hsc" #-}

  -- * Signal sets
  SignalSet,
  emptySignalSet, fullSignalSet, 
  addSignal, deleteSignal, inSignalSet,

  -- * The process signal mask
  getSignalMask, setSignalMask, blockSignals, unblockSignals,

  -- * The alarm timer
  scheduleAlarm,

  -- * Waiting for signals
  getPendingSignals, awaitSignal,


{-# LINE 84 "Signals.hsc" #-}

  -- MISSING FUNCTIONALITY:
  -- sigaction(), (inc. the sigaction structure + flags etc.)
  -- the siginfo structure
  -- sigaltstack()
  -- sighold, sigignore, sigpause, sigrelse, sigset
  -- siginterrupt

{-# LINE 92 "Signals.hsc" #-}
  ) where


{-# LINE 97 "Signals.hsc" #-}

{-# LINE 98 "Signals.hsc" #-}

{-# LINE 99 "Signals.hsc" #-}

import Foreign
import Foreign.C
import System.IO.Unsafe
import System.Posix.Types
import System.Posix.Internals


{-# LINE 107 "Signals.hsc" #-}
-- WHOLE FILE...

-- -----------------------------------------------------------------------------
-- Specific signals

type Signal = CInt

nullSignal :: Signal
nullSignal = 0


{-# LINE 118 "Signals.hsc" #-}
sigABRT   = (6)   :: CInt
{-# LINE 119 "Signals.hsc" #-}
sigALRM   = (14)   :: CInt
{-# LINE 120 "Signals.hsc" #-}
sigBUS    = (7)    :: CInt
{-# LINE 121 "Signals.hsc" #-}
sigCHLD   = (17)   :: CInt
{-# LINE 122 "Signals.hsc" #-}
sigCONT   = (18)   :: CInt
{-# LINE 123 "Signals.hsc" #-}
sigFPE    = (8)    :: CInt
{-# LINE 124 "Signals.hsc" #-}
sigHUP    = (1)    :: CInt
{-# LINE 125 "Signals.hsc" #-}
sigILL    = (4)    :: CInt
{-# LINE 126 "Signals.hsc" #-}
sigINT    = (2)    :: CInt
{-# LINE 127 "Signals.hsc" #-}
sigKILL   = (9)   :: CInt
{-# LINE 128 "Signals.hsc" #-}
sigPIPE   = (13)   :: CInt
{-# LINE 129 "Signals.hsc" #-}
sigQUIT   = (3)   :: CInt
{-# LINE 130 "Signals.hsc" #-}
sigSEGV   = (11)   :: CInt
{-# LINE 131 "Signals.hsc" #-}
sigSTOP   = (19)   :: CInt
{-# LINE 132 "Signals.hsc" #-}
sigTERM   = (15)   :: CInt
{-# LINE 133 "Signals.hsc" #-}
sigTSTP   = (20)   :: CInt
{-# LINE 134 "Signals.hsc" #-}
sigTTIN   = (21)   :: CInt
{-# LINE 135 "Signals.hsc" #-}
sigTTOU   = (22)   :: CInt
{-# LINE 136 "Signals.hsc" #-}
sigUSR1   = (10)   :: CInt
{-# LINE 137 "Signals.hsc" #-}
sigUSR2   = (12)   :: CInt
{-# LINE 138 "Signals.hsc" #-}

{-# LINE 139 "Signals.hsc" #-}
sigPOLL   = (29)   :: CInt
{-# LINE 140 "Signals.hsc" #-}

{-# LINE 141 "Signals.hsc" #-}
sigPROF   = (27)   :: CInt
{-# LINE 142 "Signals.hsc" #-}
sigSYS    = (31)    :: CInt
{-# LINE 143 "Signals.hsc" #-}
sigTRAP   = (5)   :: CInt
{-# LINE 144 "Signals.hsc" #-}
sigURG    = (23)    :: CInt
{-# LINE 145 "Signals.hsc" #-}
sigVTALRM = (26) :: CInt
{-# LINE 146 "Signals.hsc" #-}
sigXCPU   = (24)   :: CInt
{-# LINE 147 "Signals.hsc" #-}
sigXFSZ   = (25)   :: CInt
{-# LINE 148 "Signals.hsc" #-}

{-# LINE 180 "Signals.hsc" #-}

internalAbort ::Signal
internalAbort = sigABRT

realTimeAlarm :: Signal
realTimeAlarm = sigALRM

busError :: Signal
busError = sigBUS

processStatusChanged :: Signal
processStatusChanged = sigCHLD


{-# LINE 194 "Signals.hsc" #-}
continueProcess :: Signal
continueProcess = sigCONT

{-# LINE 197 "Signals.hsc" #-}

floatingPointException :: Signal
floatingPointException = sigFPE

lostConnection :: Signal
lostConnection = sigHUP

illegalInstruction :: Signal
illegalInstruction = sigILL

keyboardSignal :: Signal
keyboardSignal = sigINT

killProcess :: Signal
killProcess = sigKILL

openEndedPipe :: Signal
openEndedPipe = sigPIPE

keyboardTermination :: Signal
keyboardTermination = sigQUIT

segmentationViolation :: Signal
segmentationViolation = sigSEGV

softwareStop :: Signal
softwareStop = sigSTOP

softwareTermination :: Signal
softwareTermination = sigTERM

keyboardStop :: Signal
keyboardStop = sigTSTP

backgroundRead :: Signal
backgroundRead = sigTTIN

backgroundWrite :: Signal
backgroundWrite = sigTTOU

userDefinedSignal1 :: Signal
userDefinedSignal1 = sigUSR1

userDefinedSignal2 :: Signal
userDefinedSignal2 = sigUSR2


{-# LINE 244 "Signals.hsc" #-}
pollableEvent :: Signal
pollableEvent = sigPOLL

{-# LINE 247 "Signals.hsc" #-}

profilingTimerExpired :: Signal
profilingTimerExpired = sigPROF

badSystemCall :: Signal
badSystemCall = sigSYS

breakpointTrap :: Signal
breakpointTrap = sigTRAP

urgentDataAvailable :: Signal
urgentDataAvailable = sigURG

virtualTimerExpired :: Signal
virtualTimerExpired = sigVTALRM

cpuTimeLimitExceeded :: Signal
cpuTimeLimitExceeded = sigXCPU

fileSizeLimitExceeded :: Signal
fileSizeLimitExceeded = sigXFSZ

-- -----------------------------------------------------------------------------
-- Signal-related functions

signalProcess :: Signal -> ProcessID -> IO ()
signalProcess sig pid 
 = throwErrnoIfMinus1_ "signalProcess" (c_kill (fromIntegral pid) sig)

foreign import ccall unsafe "Signals_inc.h kill"
  c_kill :: CPid -> CInt -> IO CInt

signalProcessGroup :: Signal -> ProcessGroupID -> IO ()
signalProcessGroup sig pgid 
  = throwErrnoIfMinus1_ "signalProcessGroup" (c_killpg (fromIntegral pgid) sig)

foreign import ccall unsafe "Signals_inc.h killpg"
  c_killpg :: CPid -> CInt -> IO CInt

raiseSignal :: Signal -> IO ()
raiseSignal sig = throwErrnoIfMinus1_ "raiseSignal" (c_raise sig)

foreign import ccall unsafe "Signals_inc.h raise"
  c_raise :: CInt -> IO CInt


{-# LINE 349 "Signals.hsc" #-}

-- -----------------------------------------------------------------------------
-- Alarms

scheduleAlarm :: Int -> IO Int
scheduleAlarm secs = do
   r <- c_alarm (fromIntegral secs)
   return (fromIntegral r)

foreign import ccall unsafe "Signals_inc.h alarm"
  c_alarm :: CUInt -> IO CUInt


{-# LINE 383 "Signals.hsc" #-}

-- -----------------------------------------------------------------------------
-- Manipulating signal sets

newtype SignalSet = SignalSet (ForeignPtr CSigset)

emptySignalSet :: SignalSet
emptySignalSet = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes sizeof_sigset_t
  throwErrnoIfMinus1_ "emptySignalSet" (withForeignPtr fp $ c_sigemptyset)
  return (SignalSet fp)

fullSignalSet :: SignalSet
fullSignalSet = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes sizeof_sigset_t
  throwErrnoIfMinus1_ "fullSignalSet" (withForeignPtr fp $ c_sigfillset)
  return (SignalSet fp)

infixr `addSignal`, `deleteSignal`
addSignal :: Signal -> SignalSet -> SignalSet
addSignal sig (SignalSet fp1) = unsafePerformIO $ do
  fp2 <- mallocForeignPtrBytes sizeof_sigset_t
  withForeignPtr fp1 $ \p1 ->
    withForeignPtr fp2 $ \p2 -> do
      copyBytes p2 p1 sizeof_sigset_t
      throwErrnoIfMinus1_ "addSignal" (c_sigaddset p2 sig)
  return (SignalSet fp2)

deleteSignal :: Signal -> SignalSet -> SignalSet
deleteSignal sig (SignalSet fp1) = unsafePerformIO $ do
  fp2 <- mallocForeignPtrBytes sizeof_sigset_t
  withForeignPtr fp1 $ \p1 ->
    withForeignPtr fp2 $ \p2 -> do
      copyBytes p2 p1 sizeof_sigset_t
      throwErrnoIfMinus1_ "deleteSignal" (c_sigdelset p2 sig)
  return (SignalSet fp2)

inSignalSet :: Signal -> SignalSet -> Bool
inSignalSet sig (SignalSet fp) = unsafePerformIO $
  withForeignPtr fp $ \p -> do
    r <- throwErrnoIfMinus1 "inSignalSet" (c_sigismember p sig)
    return (r /= 0)

getSignalMask :: IO SignalSet
getSignalMask = do
  fp <- mallocForeignPtrBytes sizeof_sigset_t
  withForeignPtr fp $ \p ->
    throwErrnoIfMinus1_ "getSignalMask" (c_sigprocmask 0 nullPtr p)
  return (SignalSet fp)
   
sigProcMask :: String -> CInt -> SignalSet -> IO ()
sigProcMask fn how (SignalSet set) =
  withForeignPtr set $ \p_set ->
    throwErrnoIfMinus1_ fn (c_sigprocmask how p_set nullPtr)
  
setSignalMask :: SignalSet -> IO ()
setSignalMask set = sigProcMask "setSignalMask" c_SIG_SETMASK set

blockSignals :: SignalSet -> IO ()
blockSignals set = sigProcMask "blockSignals" c_SIG_BLOCK set

unblockSignals :: SignalSet -> IO ()
unblockSignals set = sigProcMask "unblockSignals" c_SIG_UNBLOCK set

getPendingSignals :: IO SignalSet
getPendingSignals = do
  fp <- mallocForeignPtrBytes sizeof_sigset_t
  withForeignPtr fp $ \p -> 
   throwErrnoIfMinus1_ "getPendingSignals" (c_sigpending p)
  return (SignalSet fp)


{-# LINE 455 "Signals.hsc" #-}
awaitSignal :: Maybe SignalSet -> IO ()
awaitSignal maybe_sigset = do
  fp <- case maybe_sigset of
    	  Nothing -> do SignalSet fp <- getSignalMask; return fp
    	  Just (SignalSet fp) -> return fp
  withForeignPtr fp $ \p -> do
    c_sigsuspend p
    return ()
    -- ignore the return value; according to the docs it can only ever be
    -- (-1) with errno set to EINTR.
 
foreign import ccall unsafe "Signals_inc.h sigsuspend"
  c_sigsuspend :: Ptr CSigset -> IO CInt

{-# LINE 469 "Signals.hsc" #-}


{-# LINE 471 "Signals.hsc" #-}
foreign import ccall unsafe "Signals_inc.h sigdelset"
  c_sigdelset   :: Ptr CSigset -> CInt -> IO CInt

foreign import ccall unsafe "Signals_inc.h sigfillset"
  c_sigfillset  :: Ptr CSigset -> IO CInt

foreign import ccall unsafe "Signals_inc.h sigismember"
  c_sigismember :: Ptr CSigset -> CInt -> IO CInt

{-# LINE 489 "Signals.hsc" #-}

foreign import ccall unsafe "Signals_inc.h sigpending"
  c_sigpending :: Ptr CSigset -> IO CInt


{-# LINE 494 "Signals.hsc" #-}
c_SIG_BLOCK   = (0)   :: CInt
{-# LINE 495 "Signals.hsc" #-}
c_SIG_SETMASK = (2) :: CInt
{-# LINE 496 "Signals.hsc" #-}
c_SIG_UNBLOCK = (1) :: CInt
{-# LINE 497 "Signals.hsc" #-}

{-# LINE 502 "Signals.hsc" #-}


{-# LINE 504 "Signals.hsc" #-}

