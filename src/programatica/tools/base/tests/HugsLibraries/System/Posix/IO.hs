{-# OPTIONS -#include "HsUnix.h" #-}
{-# LINE 1 "IO.hsc" #-}
{-# OPTIONS -fffi #-}
{-# LINE 2 "IO.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.IO
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX IO support
--
-----------------------------------------------------------------------------

module System.Posix.IO (
    -- * Input \/ Output

    -- ** Standard file descriptors
    stdInput, stdOutput, stdError,

    -- ** Opening and closing files
    OpenMode(..),
    OpenFileFlags(..), defaultFileFlags,
    openFd, createFile,
    closeFd,

    -- ** Reading\/writing data
    -- |Programmers using the 'fdRead' and 'fdWrite' API should be aware that
    -- EAGAIN exceptions may occur for non-blocking IO!

    fdRead, fdWrite,

    -- ** Seeking
    fdSeek,

    -- ** File options
    FdOption(..),
    queryFdOption,
    setFdOption,

    -- ** Locking
    FileLock,
    LockRequest(..),
    getLock,  setLock,
    waitToSetLock,

    -- ** Pipes
    createPipe,

    -- ** Duplicating file descriptors
    dup, dupTo,

    -- ** Converting file descriptors to\/from Handles
    handleToFd,
    fdToHandle,  

  ) where

import System.IO
import System.IO.Error
import System.Posix.Types
import System.Posix.Internals

import Foreign
import Foreign.C
import Data.Bits


{-# LINE 74 "IO.hsc" #-}


{-# LINE 76 "IO.hsc" #-}
import Hugs.Prelude (IOException(..), IOErrorType(..))
import qualified Hugs.IO (handleToFd, openFd)

{-# LINE 79 "IO.hsc" #-}


{-# LINE 81 "IO.hsc" #-}

-- -----------------------------------------------------------------------------
-- Pipes
-- |The 'createPipe' function creates a pair of connected file descriptors. The first
-- component is the fd to read from, the second is the write end.
-- Although pipes may be bidirectional, this behaviour is not portable and
-- programmers should use two separate pipes for this purpose.

createPipe :: IO (Fd, Fd)
createPipe =
  allocaArray 2 $ \p_fd -> do
    throwErrnoIfMinus1_ "createPipe" (c_pipe p_fd)
    rfd <- peekElemOff p_fd 0
    wfd <- peekElemOff p_fd 1
    return (Fd rfd, Fd wfd)

-- -----------------------------------------------------------------------------
-- Duplicating file descriptors

dup :: Fd -> IO Fd
dup (Fd fd) = do r <- throwErrnoIfMinus1 "dup" (c_dup fd); return (Fd r)

dupTo :: Fd -> Fd -> IO Fd
dupTo (Fd fd1) (Fd fd2) = do
  r <- throwErrnoIfMinus1 "dupTo" (c_dup2 fd1 fd2)
  return (Fd r)

-- -----------------------------------------------------------------------------
-- Opening and closing files

stdInput, stdOutput, stdError :: Fd
stdInput   = Fd (0)
{-# LINE 113 "IO.hsc" #-}
stdOutput  = Fd (1)
{-# LINE 114 "IO.hsc" #-}
stdError   = Fd (2)
{-# LINE 115 "IO.hsc" #-}

data OpenMode = ReadOnly | WriteOnly | ReadWrite

data OpenFileFlags =
 OpenFileFlags {
    append    :: Bool,
    exclusive :: Bool,
    noctty    :: Bool,
    nonBlock  :: Bool,
    trunc     :: Bool
 }

defaultFileFlags :: OpenFileFlags
defaultFileFlags =
 OpenFileFlags {
    append    = False,
    exclusive = False,
    noctty    = False,
    nonBlock  = False,
    trunc     = False
  }

openFd :: FilePath
       -> OpenMode
       -> Maybe FileMode -- Just x => O_CREAT, Nothing => must exist
       -> OpenFileFlags
       -> IO Fd
openFd name how maybe_mode (OpenFileFlags append exclusive noctty
				nonBlock truncate) = do
   withCString name $ \s -> do
    fd <- throwErrnoIfMinus1 "openFd" (c_open s all_flags mode_w)
    return (Fd fd)
  where
    all_flags  = creat .|. flags .|. open_mode

    flags =
       (if append    then (1024)   else 0) .|.
{-# LINE 152 "IO.hsc" #-}
       (if exclusive then (128)     else 0) .|.
{-# LINE 153 "IO.hsc" #-}
       (if noctty    then (256)   else 0) .|.
{-# LINE 154 "IO.hsc" #-}
       (if nonBlock  then (2048) else 0) .|.
{-# LINE 155 "IO.hsc" #-}
       (if truncate  then (512)    else 0)
{-# LINE 156 "IO.hsc" #-}

    (creat, mode_w) = case maybe_mode of 
			Nothing -> (0,0)
			Just x  -> ((64), x)
{-# LINE 160 "IO.hsc" #-}

    open_mode = case how of
		   ReadOnly  -> (0)
{-# LINE 163 "IO.hsc" #-}
		   WriteOnly -> (1)
{-# LINE 164 "IO.hsc" #-}
		   ReadWrite -> (2)
{-# LINE 165 "IO.hsc" #-}

createFile :: FilePath -> FileMode -> IO Fd
createFile name mode
  = openFd name WriteOnly (Just mode) defaultFileFlags{ trunc=True } 

closeFd :: Fd -> IO ()
closeFd (Fd fd) = throwErrnoIfMinus1_ "closeFd" (c_close fd)

-- -----------------------------------------------------------------------------
-- Converting file descriptors to/from Handles


{-# LINE 193 "IO.hsc" #-}


{-# LINE 195 "IO.hsc" #-}
handleToFd :: Handle -> IO Fd
handleToFd h = do
  fd <- Hugs.IO.handleToFd h
  return (fromIntegral fd)

fdToHandle :: Fd -> IO Handle
fdToHandle fd = do
  mode <- fdGetMode (fromIntegral fd)
  Hugs.IO.openFd (fromIntegral fd) False mode True

{-# LINE 205 "IO.hsc" #-}

-- -----------------------------------------------------------------------------
-- Fd options

data FdOption = AppendOnWrite
	      | CloseOnExec
	      | NonBlockingRead
	      | SynchronousWrites

fdOption2Int :: FdOption -> CInt
fdOption2Int CloseOnExec       = (1)
{-# LINE 216 "IO.hsc" #-}
fdOption2Int AppendOnWrite     = (1024)
{-# LINE 217 "IO.hsc" #-}
fdOption2Int NonBlockingRead   = (2048)
{-# LINE 218 "IO.hsc" #-}
fdOption2Int SynchronousWrites = (4096)
{-# LINE 219 "IO.hsc" #-}

queryFdOption :: Fd -> FdOption -> IO Bool
queryFdOption (Fd fd) opt = do
  r <- throwErrnoIfMinus1 "queryFdOption" (c_fcntl_read fd flag)
  return (testBit r (fromIntegral (fdOption2Int opt)))
 where
  flag    = case opt of
	      CloseOnExec       -> (1)
{-# LINE 227 "IO.hsc" #-}
	      other		-> (3)
{-# LINE 228 "IO.hsc" #-}

setFdOption :: Fd -> FdOption -> Bool -> IO ()
setFdOption (Fd fd) opt val = do
  r <- throwErrnoIfMinus1 "setFdOption" (c_fcntl_read fd getflag)
  let r' | val       = r .|. opt_val
	 | otherwise = r .&. (complement opt_val)
  throwErrnoIfMinus1_ "setFdOption" (c_fcntl_write fd setflag r')
 where
  (getflag,setflag)= case opt of
	      CloseOnExec       -> ((1),(2)) 
{-# LINE 238 "IO.hsc" #-}
	      other		-> ((3),(4))
{-# LINE 239 "IO.hsc" #-}
  opt_val = fdOption2Int opt

-- -----------------------------------------------------------------------------
-- Seeking 

mode2Int :: SeekMode -> CInt
mode2Int AbsoluteSeek = (0)
{-# LINE 246 "IO.hsc" #-}
mode2Int RelativeSeek = (1)
{-# LINE 247 "IO.hsc" #-}
mode2Int SeekFromEnd  = (2)
{-# LINE 248 "IO.hsc" #-}

fdSeek :: Fd -> SeekMode -> FileOffset -> IO FileOffset
fdSeek (Fd fd) mode off =
  throwErrnoIfMinus1 "fdSeek" (c_lseek fd off (mode2Int mode))

-- -----------------------------------------------------------------------------
-- Locking

data LockRequest = ReadLock
                 | WriteLock
                 | Unlock

type FileLock = (LockRequest, SeekMode, FileOffset, FileOffset)

getLock :: Fd -> FileLock -> IO (Maybe (ProcessID, FileLock))
getLock (Fd fd) lock =
  allocaLock lock $ \p_flock -> do
    throwErrnoIfMinus1_ "getLock" (c_fcntl_lock fd (5) p_flock)
{-# LINE 266 "IO.hsc" #-}
    result <- bytes2ProcessIDAndLock p_flock
    return (maybeResult result)
  where
    maybeResult (_, (Unlock, _, _, _)) = Nothing
    maybeResult x = Just x

allocaLock :: FileLock -> (Ptr CFLock -> IO a) -> IO a
allocaLock (lockreq, mode, start, len) io = 
  allocaBytes (16) $ \p -> do
{-# LINE 275 "IO.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0))   p (lockReq2Int lockreq :: CShort)
{-# LINE 276 "IO.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 2)) p (fromIntegral (mode2Int mode) :: CShort)
{-# LINE 277 "IO.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4))  p start
{-# LINE 278 "IO.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8))    p len
{-# LINE 279 "IO.hsc" #-}
    io p

lockReq2Int :: LockRequest -> CShort
lockReq2Int ReadLock  = (0)
{-# LINE 283 "IO.hsc" #-}
lockReq2Int WriteLock = (1)
{-# LINE 284 "IO.hsc" #-}
lockReq2Int Unlock    = (2)
{-# LINE 285 "IO.hsc" #-}

bytes2ProcessIDAndLock :: Ptr CFLock -> IO (ProcessID, FileLock)
bytes2ProcessIDAndLock p = do
  req   <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))   p
{-# LINE 289 "IO.hsc" #-}
  mode  <- ((\hsc_ptr -> peekByteOff hsc_ptr 2)) p
{-# LINE 290 "IO.hsc" #-}
  start <- ((\hsc_ptr -> peekByteOff hsc_ptr 4))  p
{-# LINE 291 "IO.hsc" #-}
  len   <- ((\hsc_ptr -> peekByteOff hsc_ptr 8))    p
{-# LINE 292 "IO.hsc" #-}
  pid   <- ((\hsc_ptr -> peekByteOff hsc_ptr 12))    p
{-# LINE 293 "IO.hsc" #-}
  return (pid, (int2req req, int2mode mode, start, len))
 where
  int2req :: CShort -> LockRequest
  int2req (0) = ReadLock
{-# LINE 297 "IO.hsc" #-}
  int2req (1) = WriteLock
{-# LINE 298 "IO.hsc" #-}
  int2req (2) = Unlock
{-# LINE 299 "IO.hsc" #-}
  int2req _ = error $ "int2req: bad argument"

  int2mode :: CShort -> SeekMode
  int2mode (0) = AbsoluteSeek
{-# LINE 303 "IO.hsc" #-}
  int2mode (1) = RelativeSeek
{-# LINE 304 "IO.hsc" #-}
  int2mode (2) = SeekFromEnd
{-# LINE 305 "IO.hsc" #-}
  int2mode _ = error $ "int2mode: bad argument"

setLock :: Fd -> FileLock -> IO ()
setLock (Fd fd) lock = do
  allocaLock lock $ \p_flock ->
    throwErrnoIfMinus1_ "setLock" (c_fcntl_lock fd (6) p_flock)
{-# LINE 311 "IO.hsc" #-}

waitToSetLock :: Fd -> FileLock -> IO ()
waitToSetLock (Fd fd) lock = do
  allocaLock lock $ \p_flock ->
    throwErrnoIfMinus1_ "waitToSetLock" 
	(c_fcntl_lock fd (7) p_flock)
{-# LINE 317 "IO.hsc" #-}

-- -----------------------------------------------------------------------------
-- fd{Read,Write}

fdRead :: Fd -> ByteCount -> IO (String, ByteCount)
fdRead _fd 0 = return ("", 0)
fdRead (Fd fd) nbytes = do
    allocaBytes (fromIntegral nbytes) $ \ bytes -> do
      rc    <-  throwErrnoIfMinus1Retry "fdRead" (c_read fd bytes nbytes)
      case fromIntegral rc of
	0 -> ioError (IOError Nothing EOF "fdRead" "EOF" Nothing)
	n -> do
	 s <- peekCStringLen (bytes, fromIntegral n)
	 return (s, n)

fdWrite :: Fd -> String -> IO ByteCount
fdWrite (Fd fd) str = withCStringLen str $ \ (strPtr,len) -> do
    rc <- throwErrnoIfMinus1Retry "fdWrite" (c_write fd strPtr (fromIntegral len))
    return (fromIntegral rc)
