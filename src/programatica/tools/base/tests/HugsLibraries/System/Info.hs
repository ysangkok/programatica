-----------------------------------------------------------------------------
-- |
-- Module      :  System.Info
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Misc information about the characteristics of the host 
-- architecture\/machine lucky enough to run your program.
--
-----------------------------------------------------------------------------

module System.Info
   (
       os,		    -- :: String
       arch		    -- :: String
   ) where

import Prelude

arch :: String
arch = "i686"

os :: String
os = "linux"

