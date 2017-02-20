{-# LINE 1 "User.hsc" #-}
{-# LINE 2 "User.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.User
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX user\/group support
--
-----------------------------------------------------------------------------

module System.Posix.User (
    -- * User environment
    -- ** Querying the user environment
    getRealUserID,
    getRealGroupID,
    getEffectiveUserID,
    getEffectiveGroupID,
    getGroups,
    getLoginName,
    getEffectiveUserName,

    -- *** The group database
    GroupEntry(..),
    getGroupEntryForID,
    getGroupEntryForName,

    -- *** The user database
    UserEntry(..),
    getUserEntryForID,
    getUserEntryForName,

    -- ** Modifying the user environment
    setUserID,
    setGroupID,

  ) where


{-# LINE 44 "User.hsc" #-}


{-# LINE 49 "User.hsc" #-}

import System.Posix.Types
import Foreign
import Foreign.C
import System.Posix.Internals	( CGroup, CPasswd )

-- -----------------------------------------------------------------------------
-- user environemnt

getRealUserID :: IO UserID
getRealUserID = c_getuid

foreign import ccall unsafe "HsUnix.h getuid"
  c_getuid :: IO CUid

getRealGroupID :: IO GroupID
getRealGroupID = c_getgid

foreign import ccall unsafe "HsUnix.h getgid"
  c_getgid :: IO CGid

getEffectiveUserID :: IO UserID
getEffectiveUserID = c_geteuid

foreign import ccall unsafe "HsUnix.h geteuid"
  c_geteuid :: IO CUid

getEffectiveGroupID :: IO GroupID
getEffectiveGroupID = c_getegid

foreign import ccall unsafe "HsUnix.h getegid"
  c_getegid :: IO CGid

getGroups :: IO [GroupID]
getGroups = do
    ngroups <- c_getgroups 0 nullPtr
    allocaArray (fromIntegral ngroups) $ \arr -> do
       throwErrnoIfMinus1_ "getGroups" (c_getgroups ngroups arr)
       groups <- peekArray (fromIntegral ngroups) arr
       return groups

foreign import ccall unsafe "HsUnix.h getgroups"
  c_getgroups :: CInt -> Ptr CGid -> IO CInt

-- ToDo: use getlogin_r
getLoginName :: IO String
getLoginName =  do
    str <- throwErrnoIfNull "getLoginName" c_getlogin
    peekCString str

foreign import ccall unsafe "HsUnix.h getlogin"
  c_getlogin :: IO CString

setUserID :: UserID -> IO ()
setUserID uid = throwErrnoIfMinus1_ "setUserID" (c_setuid uid)

foreign import ccall unsafe "HsUnix.h setuid"
  c_setuid :: CUid -> IO CInt

setGroupID :: GroupID -> IO ()
setGroupID gid = throwErrnoIfMinus1_ "setGroupID" (c_setgid gid)

foreign import ccall unsafe "HsUnix.h setgid"
  c_setgid :: CGid -> IO CInt

-- -----------------------------------------------------------------------------
-- User names

getEffectiveUserName :: IO String
getEffectiveUserName = do
    euid <- getEffectiveUserID
    pw <- getUserEntryForID euid
    return (userName pw)

-- -----------------------------------------------------------------------------
-- The group database (grp.h)

data GroupEntry =
 GroupEntry {
  groupName    :: String,
  groupID      :: GroupID,
  groupMembers :: [String]
 }

getGroupEntryForID :: GroupID -> IO GroupEntry

{-# LINE 148 "User.hsc" #-}
getGroupEntryForID = error "System.Posix.User.getGroupEntryForID: not supported"

{-# LINE 150 "User.hsc" #-}


getGroupEntryForName :: String -> IO GroupEntry

{-# LINE 167 "User.hsc" #-}
getGroupEntryForName = error "System.Posix.User.getGroupEntryForName: not supported"

{-# LINE 169 "User.hsc" #-}


{-# LINE 179 "User.hsc" #-}

unpackGroupEntry :: Ptr CGroup -> IO GroupEntry
unpackGroupEntry ptr = do
   name    <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr >>= peekCString
{-# LINE 183 "User.hsc" #-}
   gid     <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 184 "User.hsc" #-}
   mem     <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 185 "User.hsc" #-}
   members <- peekArray0 nullPtr mem >>= mapM peekCString
   return (GroupEntry name gid members)

-- -----------------------------------------------------------------------------
-- The user database (pwd.h)

data UserEntry =
 UserEntry {
   userName      :: String,
   userID        :: UserID,
   userGroupID   :: GroupID,
   homeDirectory :: String,
   userShell     :: String
 }

getUserEntryForID :: UserID -> IO UserEntry

{-# LINE 214 "User.hsc" #-}
getUserEntryForID = error "System.Posix.User.getUserEntryForID: not supported"

{-# LINE 216 "User.hsc" #-}

getUserEntryForName :: String -> IO UserEntry

{-# LINE 232 "User.hsc" #-}
getUserEntryForName = error "System.Posix.User.getUserEntryForName: not supported"

{-# LINE 234 "User.hsc" #-}


{-# LINE 244 "User.hsc" #-}


{-# LINE 249 "User.hsc" #-}

unpackUserEntry :: Ptr CPasswd -> IO UserEntry
unpackUserEntry ptr = do
   name   <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  ptr >>= peekCString
{-# LINE 253 "User.hsc" #-}
   uid    <- ((\hsc_ptr -> peekByteOff hsc_ptr 8))   ptr
{-# LINE 254 "User.hsc" #-}
   gid    <- ((\hsc_ptr -> peekByteOff hsc_ptr 12))   ptr
{-# LINE 255 "User.hsc" #-}
   dir    <- ((\hsc_ptr -> peekByteOff hsc_ptr 20))   ptr >>= peekCString
{-# LINE 256 "User.hsc" #-}
   shell  <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) ptr >>= peekCString
{-# LINE 257 "User.hsc" #-}
   return (UserEntry name uid gid dir shell)

-- Used when calling re-entrant system calls that signal their 'errno' 
-- directly through the return value.
throwErrorIfNonZero_ :: String -> IO CInt -> IO ()
throwErrorIfNonZero_ loc act = do
    rc <- act
    if (rc == 0) 
     then return ()
     else ioError (errnoToIOError loc (Errno (fromIntegral rc)) Nothing Nothing)

