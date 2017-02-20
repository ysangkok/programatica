{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tuple
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The tuple data types, and associated functions.
--
-----------------------------------------------------------------------------

module Data.Tuple
  ( fst		-- :: (a,b) -> a
  , snd		-- :: (a,b) -> a
  , curry	-- :: ((a, b) -> c) -> a -> b -> c
  , uncurry	-- :: (a -> b -> c) -> ((a, b) -> c)
  )
    where

default ()		-- Double isn't available yet

-- ---------------------------------------------------------------------------
-- Standard functions over tuples

