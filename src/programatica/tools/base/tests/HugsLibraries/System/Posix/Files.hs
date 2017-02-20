{-# LINE 1 "Files.hsc" #-}
{-# LINE 2 "Files.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Files
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX file support
--
-----------------------------------------------------------------------------

module System.Posix.Files (
    -- * File modes
    -- FileMode exported by System.Posix.Types
    unionFileModes, intersectFileModes,
    nullFileMode,
    ownerReadMode, ownerWriteMode, ownerExecuteMode, ownerModes,
    groupReadMode, groupWriteMode, groupExecuteMode, groupModes,
    otherReadMode, otherWriteMode, otherExecuteMode, otherModes,
    setUserIDMode, setGroupIDMode,
    stdFileMode,   accessModes,

    -- ** Setting file modes
    setFileMode, setFdMode, setFileCreationMask,

    -- ** Checking file existence and permissions
    fileAccess, fileExist,

    -- * File status
    FileStatus,
    -- ** Obtaining file status
    getFileStatus, getFdStatus, getSymbolicLinkStatus,
    -- ** Querying file status
    deviceID, fileID, fileMode, linkCount, fileOwner, fileGroup,
    specialDeviceID, fileSize, accessTime, modificationTime,
    statusChangeTime,
    isBlockDevice, isCharacterDevice, isNamedPipe, isRegularFile,
    isDirectory, isSymbolicLink, isSocket,

    -- * Creation
    createNamedPipe, 
    createDevice,

    -- * Hard links
    createLink, removeLink,

    -- * Symbolic links
    createSymbolicLink, readSymbolicLink,

    -- * Renaming files
    rename,

    -- * Changing file ownership
    setOwnerAndGroup,  setFdOwnerAndGroup,

{-# LINE 62 "Files.hsc" #-}

    -- * Changing file timestamps
    setFileTimes, touchFile,

    -- * Setting file sizes
    setFileSize, setFdSize,

    -- * Find system-specific limits for a file
    PathVar(..), getPathVar, getFdPathVar,
  ) where


{-# LINE 74 "Files.hsc" #-}

import System.Posix.Types
import System.IO.Unsafe
import Data.Bits
import System.Posix.Internals
import Foreign
import Foreign.C

-- -----------------------------------------------------------------------------
-- POSIX file modes

-- The abstract type 'FileMode', constants and operators for
-- manipulating the file modes defined by POSIX.

nullFileMode :: FileMode
nullFileMode = 0

ownerReadMode :: FileMode
ownerReadMode = (256)
{-# LINE 93 "Files.hsc" #-}

ownerWriteMode :: FileMode
ownerWriteMode = (128)
{-# LINE 96 "Files.hsc" #-}

ownerExecuteMode :: FileMode
ownerExecuteMode = (64)
{-# LINE 99 "Files.hsc" #-}

groupReadMode :: FileMode
groupReadMode = (32)
{-# LINE 102 "Files.hsc" #-}

groupWriteMode :: FileMode
groupWriteMode = (16)
{-# LINE 105 "Files.hsc" #-}

groupExecuteMode :: FileMode
groupExecuteMode = (8)
{-# LINE 108 "Files.hsc" #-}

otherReadMode :: FileMode
otherReadMode = (4)
{-# LINE 111 "Files.hsc" #-}

otherWriteMode :: FileMode
otherWriteMode = (2)
{-# LINE 114 "Files.hsc" #-}

otherExecuteMode :: FileMode
otherExecuteMode = (1)
{-# LINE 117 "Files.hsc" #-}

setUserIDMode :: FileMode
setUserIDMode = (2048)
{-# LINE 120 "Files.hsc" #-}

setGroupIDMode :: FileMode
setGroupIDMode = (1024)
{-# LINE 123 "Files.hsc" #-}

stdFileMode :: FileMode
stdFileMode = ownerReadMode  .|. ownerWriteMode .|. 
	      groupReadMode  .|. groupWriteMode .|. 
	      otherReadMode  .|. otherWriteMode

ownerModes :: FileMode
ownerModes = (448)
{-# LINE 131 "Files.hsc" #-}

groupModes :: FileMode
groupModes = (56)
{-# LINE 134 "Files.hsc" #-}

otherModes :: FileMode
otherModes = (7)
{-# LINE 137 "Files.hsc" #-}

accessModes :: FileMode
accessModes = ownerModes .|. groupModes .|. otherModes

unionFileModes :: FileMode -> FileMode -> FileMode
unionFileModes m1 m2 = m1 .|. m2

intersectFileModes :: FileMode -> FileMode -> FileMode
intersectFileModes m1 m2 = m1 .&. m2

-- Not exported:
fileTypeModes :: FileMode
fileTypeModes = (61440)
{-# LINE 150 "Files.hsc" #-}

blockSpecialMode :: FileMode
blockSpecialMode = (24576)
{-# LINE 153 "Files.hsc" #-}

characterSpecialMode :: FileMode
characterSpecialMode = (8192)
{-# LINE 156 "Files.hsc" #-}

namedPipeMode :: FileMode
namedPipeMode = (4096)
{-# LINE 159 "Files.hsc" #-}

regularFileMode :: FileMode
regularFileMode = (32768)
{-# LINE 162 "Files.hsc" #-}

directoryMode :: FileMode
directoryMode = (16384)
{-# LINE 165 "Files.hsc" #-}

symbolicLinkMode :: FileMode
symbolicLinkMode = (40960)
{-# LINE 168 "Files.hsc" #-}

socketMode :: FileMode
socketMode = (49152)
{-# LINE 171 "Files.hsc" #-}

setFileMode :: FilePath -> FileMode -> IO ()
setFileMode name m =
  withCString name $ \s -> do
    throwErrnoIfMinus1_ "setFileMode" (c_chmod s m)

setFdMode :: Fd -> FileMode -> IO ()
setFdMode fd m =
  throwErrnoIfMinus1_ "setFdMode" (c_fchmod fd m)

foreign import ccall unsafe "HsUnix.h fchmod" 
  c_fchmod :: Fd -> CMode -> IO CInt

setFileCreationMask :: FileMode -> IO FileMode
setFileCreationMask mask = c_umask mask

-- -----------------------------------------------------------------------------
-- access()

fileAccess :: FilePath -> Bool -> Bool -> Bool -> IO Bool
fileAccess name read write exec = access name flags
  where
   flags   = read_f .|. write_f .|. exec_f
   read_f  = if read  then (4) else 0
{-# LINE 195 "Files.hsc" #-}
   write_f = if write then (2) else 0
{-# LINE 196 "Files.hsc" #-}
   exec_f  = if exec  then (1) else 0
{-# LINE 197 "Files.hsc" #-}

fileExist :: FilePath -> IO Bool
fileExist name = 
  withCString name $ \s -> do
    r <- c_access s (0)
{-# LINE 202 "Files.hsc" #-}
    if (r == 0)
	then return True
	else do err <- getErrno
	        if (err == eNOENT)
		   then return False
		   else throwErrno "fileExist"

access :: FilePath -> CMode -> IO Bool
access name flags = 
  withCString name $ \s -> do
    r <- c_access s flags
    if (r == 0)
	then return True
	else do err <- getErrno
	        if (err == eACCES)
		   then return False
		   else throwErrno "fileAccess"

-- -----------------------------------------------------------------------------
-- stat() support

newtype FileStatus = FileStatus (ForeignPtr CStat)

deviceID         :: FileStatus -> DeviceID
fileID           :: FileStatus -> FileID
fileMode         :: FileStatus -> FileMode
linkCount        :: FileStatus -> LinkCount
fileOwner        :: FileStatus -> UserID
fileGroup        :: FileStatus -> GroupID
specialDeviceID  :: FileStatus -> DeviceID
fileSize         :: FileStatus -> FileOffset
accessTime       :: FileStatus -> EpochTime
modificationTime :: FileStatus -> EpochTime
statusChangeTime :: FileStatus -> EpochTime

deviceID (FileStatus stat) = 
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 0))
{-# LINE 239 "Files.hsc" #-}
fileID (FileStatus stat) = 
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 12))
{-# LINE 241 "Files.hsc" #-}
fileMode (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 16))
{-# LINE 243 "Files.hsc" #-}
linkCount (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 20))
{-# LINE 245 "Files.hsc" #-}
fileOwner (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 24))
{-# LINE 247 "Files.hsc" #-}
fileGroup (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 28))
{-# LINE 249 "Files.hsc" #-}
specialDeviceID (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 32))
{-# LINE 251 "Files.hsc" #-}
fileSize (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 44))
{-# LINE 253 "Files.hsc" #-}
accessTime (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 56))
{-# LINE 255 "Files.hsc" #-}
modificationTime (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 64))
{-# LINE 257 "Files.hsc" #-}
statusChangeTime (FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 72))
{-# LINE 259 "Files.hsc" #-}

isBlockDevice     :: FileStatus -> Bool
isCharacterDevice :: FileStatus -> Bool
isNamedPipe       :: FileStatus -> Bool
isRegularFile     :: FileStatus -> Bool
isDirectory       :: FileStatus -> Bool
isSymbolicLink    :: FileStatus -> Bool
isSocket          :: FileStatus -> Bool

isBlockDevice stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == blockSpecialMode
isCharacterDevice stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == characterSpecialMode
isNamedPipe stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == namedPipeMode
isRegularFile stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == regularFileMode
isDirectory stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == directoryMode
isSymbolicLink stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == symbolicLinkMode
isSocket stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == socketMode

getFileStatus :: FilePath -> IO FileStatus
getFileStatus path = do
  fp <- mallocForeignPtrBytes (88) 
{-# LINE 286 "Files.hsc" #-}
  withForeignPtr fp $ \p ->
    withCString path $ \s -> 
      throwErrnoIfMinus1_ "getFileStatus" (c_stat s p)
  return (FileStatus fp)

getFdStatus :: Fd -> IO FileStatus
getFdStatus (Fd fd) = do
  fp <- mallocForeignPtrBytes (88) 
{-# LINE 294 "Files.hsc" #-}
  withForeignPtr fp $ \p ->
    throwErrnoIfMinus1_ "getFdStatus" (c_fstat fd p)
  return (FileStatus fp)

getSymbolicLinkStatus :: FilePath -> IO FileStatus
getSymbolicLinkStatus path = do
  fp <- mallocForeignPtrBytes (88) 
{-# LINE 301 "Files.hsc" #-}
  withForeignPtr fp $ \p ->
    withCString path $ \s -> 
      throwErrnoIfMinus1_ "getSymbolicLinkStatus" (c_lstat s p)
  return (FileStatus fp)

foreign import ccall unsafe "HsUnix.h lstat" 
  c_lstat :: CString -> Ptr CStat -> IO CInt

createNamedPipe :: FilePath -> FileMode -> IO ()
createNamedPipe name mode = do
  withCString name $ \s -> 
    throwErrnoIfMinus1_ "createNamedPipe" (c_mkfifo s mode)

createDevice :: FilePath -> FileMode -> DeviceID -> IO ()
createDevice path mode dev =
  withCString path $ \s ->
    throwErrnoIfMinus1_ "createDevice" (c_mknod s mode dev)

foreign import ccall unsafe "HsUnix.h mknod" 
  c_mknod :: CString -> CMode -> CDev -> IO CInt

-- -----------------------------------------------------------------------------
-- Hard links

createLink :: FilePath -> FilePath -> IO ()
createLink name1 name2 =
  withCString name1 $ \s1 ->
  withCString name2 $ \s2 ->
  throwErrnoIfMinus1_ "createLink" (c_link s1 s2)

removeLink :: FilePath -> IO ()
removeLink name =
  withCString name $ \s ->
  throwErrnoIfMinus1_ "removeLink" (c_unlink s)

-- -----------------------------------------------------------------------------
-- Symbolic Links

createSymbolicLink :: FilePath -> FilePath -> IO ()
createSymbolicLink file1 file2 =
  withCString file1 $ \s1 ->
  withCString file2 $ \s2 ->
  throwErrnoIfMinus1_ "createSymbolicLink" (c_symlink s1 s2)

foreign import ccall unsafe "HsUnix.h symlink"
  c_symlink :: CString -> CString -> IO CInt

-- ToDo: should really use SYMLINK_MAX, but not everyone supports it yet,
-- and it seems that the intention is that SYMLINK_MAX is no larger than
-- PATH_MAX.
readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink file =
  allocaArray0 (4096) $ \buf -> do
{-# LINE 354 "Files.hsc" #-}
    withCString file $ \s -> do
      len <- throwErrnoIfMinus1 "readSymbolicLink" $ 
	c_readlink s buf (4096)
{-# LINE 357 "Files.hsc" #-}
      peekCStringLen (buf,fromIntegral len)

foreign import ccall unsafe "HsUnix.h readlink"
  c_readlink :: CString -> CString -> CInt -> IO CInt

-- -----------------------------------------------------------------------------
-- Renaming files

rename :: FilePath -> FilePath -> IO ()
rename name1 name2 =
  withCString name1 $ \s1 ->
  withCString name2 $ \s2 ->
  throwErrnoIfMinus1_ "rename" (c_rename s1 s2)

-- -----------------------------------------------------------------------------
-- chmod()

setOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO ()
setOwnerAndGroup name uid gid = do
  withCString name $ \s ->
    throwErrnoIfMinus1_ "setOwnerAndGroup" (c_chown s uid gid)

foreign import ccall unsafe "HsUnix.h chown"
  c_chown :: CString -> CUid -> CGid -> IO CInt

setFdOwnerAndGroup :: Fd -> UserID -> GroupID -> IO ()
setFdOwnerAndGroup (Fd fd) uid gid = 
  throwErrnoIfMinus1_ "setFdOwnerAndGroup" (c_fchown fd uid gid)

foreign import ccall unsafe "HsUnix.h fchown"
  c_fchown :: CInt -> CUid -> CGid -> IO CInt


{-# LINE 398 "Files.hsc" #-}

-- -----------------------------------------------------------------------------
-- utime()

setFileTimes :: FilePath -> EpochTime -> EpochTime -> IO ()
setFileTimes name atime mtime = do
  withCString name $ \s ->
   allocaBytes (8) $ \p -> do
{-# LINE 406 "Files.hsc" #-}
     ((\hsc_ptr -> pokeByteOff hsc_ptr 0))  p atime
{-# LINE 407 "Files.hsc" #-}
     ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p mtime
{-# LINE 408 "Files.hsc" #-}
     throwErrnoIfMinus1_ "setFileTimes" (c_utime s p)

touchFile :: FilePath -> IO ()
touchFile name = do
  withCString name $ \s ->
   throwErrnoIfMinus1_ "touchFile" (c_utime s nullPtr)

-- -----------------------------------------------------------------------------
-- Setting file sizes

setFileSize :: FilePath -> FileOffset -> IO ()
setFileSize file off = 
  withCString file $ \s ->
    throwErrnoIfMinus1_ "setFileSize" (c_truncate s off)

foreign import ccall unsafe "HsUnix.h truncate"
  c_truncate :: CString -> COff -> IO CInt

setFdSize :: Fd -> FileOffset -> IO ()
setFdSize fd off =
  throwErrnoIfMinus1_ "setFdSize" (c_ftruncate fd off)

foreign import ccall unsafe "HsUnix.h ftruncate"
  c_ftruncate :: Fd -> COff -> IO CInt

-- -----------------------------------------------------------------------------
-- pathconf()/fpathconf() support

data PathVar
  = FileSizeBits		  {- _PC_FILESIZEBITS     -}
  | LinkLimit                     {- _PC_LINK_MAX         -}
  | InputLineLimit                {- _PC_MAX_CANON        -}
  | InputQueueLimit               {- _PC_MAX_INPUT        -}
  | FileNameLimit                 {- _PC_NAME_MAX         -}
  | PathNameLimit                 {- _PC_PATH_MAX         -}
  | PipeBufferLimit               {- _PC_PIPE_BUF         -}
				  -- These are described as optional in POSIX:
  				  {- _PC_ALLOC_SIZE_MIN     -}
  				  {- _PC_REC_INCR_XFER_SIZE -}
  				  {- _PC_REC_MAX_XFER_SIZE  -}
  				  {- _PC_REC_MIN_XFER_SIZE  -}
 				  {- _PC_REC_XFER_ALIGN     -}
  | SymbolicLinkLimit		  {- _PC_SYMLINK_MAX      -}
  | SetOwnerAndGroupIsRestricted  {- _PC_CHOWN_RESTRICTED -}
  | FileNamesAreNotTruncated      {- _PC_NO_TRUNC         -}
  | VDisableChar		  {- _PC_VDISABLE         -}
  | AsyncIOAvailable		  {- _PC_ASYNC_IO         -}
  | PrioIOAvailable		  {- _PC_PRIO_IO          -}
  | SyncIOAvailable		  {- _PC_SYNC_IO          -}

pathVarConst :: PathVar -> CInt
pathVarConst v = case v of
	LinkLimit     			-> (0)
{-# LINE 461 "Files.hsc" #-}
	InputLineLimit			-> (1)
{-# LINE 462 "Files.hsc" #-}
	InputQueueLimit			-> (2)
{-# LINE 463 "Files.hsc" #-}
	FileNameLimit			-> (3)
{-# LINE 464 "Files.hsc" #-}
	PathNameLimit			-> (4)
{-# LINE 465 "Files.hsc" #-}
	PipeBufferLimit			-> (5)
{-# LINE 466 "Files.hsc" #-}
	SetOwnerAndGroupIsRestricted	-> (6)
{-# LINE 467 "Files.hsc" #-}
	FileNamesAreNotTruncated	-> (7)
{-# LINE 468 "Files.hsc" #-}
	VDisableChar			-> (8)
{-# LINE 469 "Files.hsc" #-}


{-# LINE 471 "Files.hsc" #-}
	SyncIOAvailable		-> (9)
{-# LINE 472 "Files.hsc" #-}

{-# LINE 475 "Files.hsc" #-}


{-# LINE 477 "Files.hsc" #-}
	AsyncIOAvailable	-> (10)
{-# LINE 478 "Files.hsc" #-}

{-# LINE 481 "Files.hsc" #-}


{-# LINE 483 "Files.hsc" #-}
	PrioIOAvailable		-> (11)
{-# LINE 484 "Files.hsc" #-}

{-# LINE 487 "Files.hsc" #-}


{-# LINE 491 "Files.hsc" #-}
	FileSizeBits		-> error "_PC_FILESIZEBITS not available"

{-# LINE 493 "Files.hsc" #-}


{-# LINE 497 "Files.hsc" #-}
	SymbolicLinkLimit	-> error "_PC_SYMLINK_MAX not available"

{-# LINE 499 "Files.hsc" #-}

getPathVar :: FilePath -> PathVar -> IO Limit
getPathVar name v = do
  withCString name $ \ nameP -> 
    throwErrnoIfMinus1 "getPathVar" $ 
      c_pathconf nameP (pathVarConst v)

foreign import ccall unsafe "HsUnix.h pathconf" 
  c_pathconf :: CString -> CInt -> IO CLong

getFdPathVar :: Fd -> PathVar -> IO Limit
getFdPathVar fd v =
    throwErrnoIfMinus1 "getFdPathVar" $ 
      c_fpathconf fd (pathVarConst v)

foreign import ccall unsafe "HsUnix.h fpathconf" 
  c_fpathconf :: Fd -> CInt -> IO CLong
