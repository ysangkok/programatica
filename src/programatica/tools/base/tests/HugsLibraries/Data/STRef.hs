-----------------------------------------------------------------------------
-- |
-- Module      :  Data.STRef
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires non-portable module ST)
--
-- Mutable references in the (strict) ST monad.
--
-----------------------------------------------------------------------------

module Data.STRef (
	-- * STRefs
	STRef,		-- abstract, instance Eq
	newSTRef,	-- :: a -> ST s (STRef s a)
	readSTRef,	-- :: STRef s a -> ST s a
	writeSTRef,	-- :: STRef s a -> a -> ST s ()
	modifySTRef	-- :: STRef s a -> (a -> a) -> ST s ()
 ) where

import Prelude

import Hugs.ST

import Data.Typeable

stRefTc = mkTyCon "STRef"; instance (Typeable a, Typeable b) => Typeable (STRef a b) where {   typeOf x = mkAppTy stRefTc [typeOf ((undefined :: STRef a b -> a) x), 			     typeOf ((undefined :: STRef a b -> b) x)] }

-- |Mutate the contents of an 'STRef'
modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = writeSTRef ref . f =<< readSTRef ref
