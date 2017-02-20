-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ST.Lazy
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires universal quantification for runST)
--
-- This module presents an identical interface to "Control.Monad.ST",
-- but the underlying implementation of the state thread is /lazy/ (in
-- the sense that (@_|_ >> a@ is not necessarily equal to @_|_@).
--
-----------------------------------------------------------------------------

module Control.Monad.ST.Lazy (
	-- * The 'ST' monad
	ST,
	runST,
	fixST,

	-- * Converting between strict and lazy 'ST'
	strictToLazyST, lazyToStrictST,

	-- * Converting 'ST' To 'IO'
	RealWorld,
	stToIO,

	-- * Unsafe operations
	unsafeInterleaveST,
	unsafeIOToST
    ) where

import Prelude

import Control.Monad.Fix

import Control.Monad.ST (RealWorld)
import qualified Control.Monad.ST as ST

import Hugs.LazyST

instance MonadFix (ST s) where
	mfix = fixST

-- ---------------------------------------------------------------------------
-- Strict <--> Lazy

unsafeIOToST :: IO a -> ST s a
unsafeIOToST = strictToLazyST . ST.unsafeIOToST

-- | A monad transformer embedding lazy state transformers in the 'IO'
-- monad.  The 'RealWorld' parameter indicates that the internal state
-- used by the 'ST' computation is a special one supplied by the 'IO'
-- monad, and thus distinct from those used by invocations of 'runST'.
stToIO :: ST RealWorld a -> IO a
stToIO = ST.stToIO . lazyToStrictST
