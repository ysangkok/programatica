-----------------------------------------------------------------------------
-- |
-- Module      :  System.Directory
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- System-independent interface to directory manipulation.
--
-----------------------------------------------------------------------------

module System.Directory 
   ( 
    -- $intro

    -- * Actions on directories
      createDirectory		-- :: FilePath -> IO ()
    , removeDirectory		-- :: FilePath -> IO ()
    , renameDirectory		-- :: FilePath -> FilePath -> IO ()

    , getDirectoryContents      -- :: FilePath -> IO [FilePath]
    , getCurrentDirectory       -- :: IO FilePath
    , setCurrentDirectory       -- :: FilePath -> IO ()

    -- * Actions on files
    , removeFile		-- :: FilePath -> IO ()
    , renameFile                -- :: FilePath -> FilePath -> IO ()

    -- * Existence tests
    , doesFileExist		-- :: FilePath -> IO Bool
    , doesDirectoryExist        -- :: FilePath -> IO Bool

    -- * Permissions

    -- $permissions

    , Permissions(
	Permissions,
	readable,		-- :: Permissions -> Bool
	writable,		-- :: Permissions -> Bool
	executable,		-- :: Permissions -> Bool
	searchable		-- :: Permissions -> Bool
      )

    , getPermissions            -- :: FilePath -> IO Permissions
    , setPermissions	        -- :: FilePath -> Permissions -> IO ()

    -- * Timestamps

    , getModificationTime       -- :: FilePath -> IO ClockTime
   ) where

import Hugs.Directory

