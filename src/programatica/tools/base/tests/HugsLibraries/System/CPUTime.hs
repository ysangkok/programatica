{-# LINE 1 "CPUTime.hsc" #-}
-----------------------------------------------------------------------------
{-# LINE 2 "CPUTime.hsc" #-}
-- |
-- Module      :  System.CPUTime
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The standard CPUTime library.
--
-----------------------------------------------------------------------------

module System.CPUTime 
	(
         getCPUTime,       -- :: IO Integer
	 cpuTimePrecision  -- :: Integer
        ) where

import Prelude

import Data.Ratio


{-# LINE 26 "CPUTime.hsc" #-}
import Hugs.Time ( getCPUTime, clockTicks )

{-# LINE 28 "CPUTime.hsc" #-}


{-# LINE 35 "CPUTime.hsc" #-}


{-# LINE 123 "CPUTime.hsc" #-}

-- |The 'cpuTimePrecision' constant is the smallest measurable difference
-- in CPU time that the implementation can record, and is given as an
-- integral number of picoseconds.

cpuTimePrecision :: Integer
cpuTimePrecision = round ((1000000000000::Integer) % fromIntegral (clockTicks))


{-# LINE 141 "CPUTime.hsc" #-}
