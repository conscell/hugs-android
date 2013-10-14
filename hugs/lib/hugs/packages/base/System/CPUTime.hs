{-# OPTIONS_GHC -optc-D__HUGS__ #-}
{-# LINE 1 "System/CPUTime.hsc" #-}
-----------------------------------------------------------------------------
{-# LINE 2 "System/CPUTime.hsc" #-}
-- |
-- Module      :  System.CPUTime
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
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


{-# LINE 26 "System/CPUTime.hsc" #-}
import Hugs.Time ( getCPUTime, clockTicks )

{-# LINE 28 "System/CPUTime.hsc" #-}


{-# LINE 32 "System/CPUTime.hsc" #-}


{-# LINE 39 "System/CPUTime.hsc" #-}


{-# LINE 128 "System/CPUTime.hsc" #-}

-- |The 'cpuTimePrecision' constant is the smallest measurable difference
-- in CPU time that the implementation can record, and is given as an
-- integral number of picoseconds.


{-# LINE 134 "System/CPUTime.hsc" #-}
cpuTimePrecision :: Integer
cpuTimePrecision = round ((1000000000000::Integer) % fromIntegral (clockTicks))

{-# LINE 137 "System/CPUTime.hsc" #-}


{-# LINE 148 "System/CPUTime.hsc" #-}
