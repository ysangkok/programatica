{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Odds and ends, mostly functions for reading and showing
-- RealFloat-like kind of values.
--
-----------------------------------------------------------------------------

module Numeric (

        fromRat,          -- :: (RealFloat a) => Rational -> a
	showSigned,       -- :: (Real a) => (a -> ShowS) -> Int -> a -> ShowS
	readSigned,       -- :: (Real a) => ReadS a -> ReadS a

	readInt,          -- :: (Integral a) => a -> (Char -> Bool)
			  --         -> (Char -> Int) -> ReadS a
	readDec,          -- :: (Integral a) => ReadS a
	readOct,          -- :: (Integral a) => ReadS a
	readHex,          -- :: (Integral a) => ReadS a

	showInt,          -- :: Integral a => a -> ShowS
        showIntAtBase,    -- :: Integral a => a -> (a -> Char) -> a -> ShowS
        showHex,          -- :: Integral a => a -> ShowS
        showOct,          -- :: Integral a => a -> ShowS

	showEFloat,       -- :: (RealFloat a) => Maybe Int -> a -> ShowS
	showFFloat,       -- :: (RealFloat a) => Maybe Int -> a -> ShowS
	showGFloat,       -- :: (RealFloat a) => Maybe Int -> a -> ShowS
	showFloat,        -- :: (RealFloat a) => a -> ShowS
	readFloat,        -- :: (RealFloat a) => ReadS a
	
	floatToDigits,    -- :: (RealFloat a) => Integer -> a -> ([Int], Int)
	lexDigits,        -- :: ReadS String

	) where

import Data.Char

import Hugs.Prelude
import Hugs.Numeric

-- ---------------------------------------------------------------------------
-- Integer printing functions

showIntAtBase :: Integral a => a -> (Int -> Char) -> a -> ShowS
showIntAtBase base toChr n r
  | n < 0  = error ("Numeric.showIntAtBase: applied to negative number " ++ show n)
  | otherwise = 
    case quotRem n base of { (n', d) ->
    let c = toChr (fromIntegral d) in
    seq c $ -- stricter than necessary
    let
	r' = c : r
    in
    if n' == 0 then r' else showIntAtBase base toChr n' r'
    }

showHex, showOct :: Integral a => a -> ShowS
showHex = showIntAtBase 16 intToDigit
showOct = showIntAtBase 8  intToDigit
