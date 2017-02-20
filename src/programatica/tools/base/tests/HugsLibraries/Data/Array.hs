{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array 
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Basic non-strict arrays.
--
-----------------------------------------------------------------------------

module  Data.Array 

    ( 
      module Data.Ix		-- export all of Ix 
    , Array 			-- Array type is abstract

    , array	    -- :: (Ix a) => (a,a) -> [(a,b)] -> Array a b
    , listArray     -- :: (Ix a) => (a,a) -> [b] -> Array a b
    , (!)           -- :: (Ix a) => Array a b -> a -> b
    , bounds        -- :: (Ix a) => Array a b -> (a,a)
    , indices       -- :: (Ix a) => Array a b -> [a]
    , elems         -- :: (Ix a) => Array a b -> [b]
    , assocs        -- :: (Ix a) => Array a b -> [(a,b)]
    , accumArray    -- :: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [(a,c)] -> Array a b
    , (//)          -- :: (Ix a) => Array a b -> [(a,b)] -> Array a b
    , accum         -- :: (Ix a) => (b -> c -> b) -> Array a b -> [(a,c)] -> Array a b
    , ixmap         -- :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a b

    -- Array instances:
    --
    --   Ix a => Functor (Array a)
    --   (Ix a, Eq b)  => Eq   (Array a b)
    --   (Ix a, Ord b) => Ord  (Array a b)
    --   (Ix a, Show a, Show b) => Show (Array a b)
    --   (Ix a, Read a, Read b) => Read (Array a b)
    -- 

    -- Implementation checked wrt. Haskell 98 lib report, 1/99.

    ) where

import Data.Ix

import Hugs.Array

import Data.Typeable

arrayTc = mkTyCon "Array"; instance (Typeable a, Typeable b) => Typeable (Array a b) where {   typeOf x = mkAppTy arrayTc [typeOf ((undefined :: Array a b -> a) x), 			     typeOf ((undefined :: Array a b -> b) x)] }

