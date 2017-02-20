{-# LINE 1 "Temp.hsc" #-}
{-# LINE 2 "Temp.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Temp
-- Copyright   :  (c) Volker Stolz <vs@foldr.org>
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  vs@foldr.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX environment support
--
-----------------------------------------------------------------------------

module System.Posix.Temp (

	mkstemp

{- Not ported (yet?):
	tmpfile: can we handle FILE*?
	tmpnam: ISO C, should go in base?
	tempname: dito
-}

) where


{-# LINE 29 "Temp.hsc" #-}

import System.IO
import System.Posix.IO
import System.Posix.Types
import Foreign.C

-- |'mkstemp' - make a unique filename and open it for
-- reading\/writing (only safe on GHC & Hugs)

mkstemp :: String -> IO (String, Handle)
mkstemp template = do

{-# LINE 41 "Temp.hsc" #-}
  withCString template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemp" (c_mkstemp ptr)
    name <- peekCString ptr
    h <- fdToHandle fd
    return (name, h)

{-# LINE 63 "Temp.hsc" #-}

foreign import ccall unsafe "HsUnix.h mkstemp"
  c_mkstemp :: CString -> IO Fd

