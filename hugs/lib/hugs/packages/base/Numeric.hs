{-# OPTIONS_GHC -fno-implicit-prelude #-}
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
-- 'RealFloat'-like kind of values.
--
-----------------------------------------------------------------------------

module Numeric (

	-- * Showing

	showSigned,       -- :: (Real a) => (a -> ShowS) -> Int -> a -> ShowS

        showIntAtBase,    -- :: Integral a => a -> (a -> Char) -> a -> ShowS
	showInt,          -- :: Integral a => a -> ShowS
        showHex,          -- :: Integral a => a -> ShowS
        showOct,          -- :: Integral a => a -> ShowS

	showEFloat,       -- :: (RealFloat a) => Maybe Int -> a -> ShowS
	showFFloat,       -- :: (RealFloat a) => Maybe Int -> a -> ShowS
	showGFloat,       -- :: (RealFloat a) => Maybe Int -> a -> ShowS
	showFloat,        -- :: (RealFloat a) => a -> ShowS

	floatToDigits,    -- :: (RealFloat a) => Integer -> a -> ([Int], Int)

	-- * Reading

	-- | /NB:/ 'readInt' is the \'dual\' of 'showIntAtBase',
	-- and 'readDec' is the \`dual\' of 'showInt'.
	-- The inconsistent naming is a historical accident.

	readSigned,       -- :: (Real a) => ReadS a -> ReadS a

	readInt,          -- :: (Integral a) => a -> (Char -> Bool)
			  --         -> (Char -> Int) -> ReadS a
	readDec,          -- :: (Integral a) => ReadS a
	readOct,          -- :: (Integral a) => ReadS a
	readHex,          -- :: (Integral a) => ReadS a

	readFloat,        -- :: (RealFloat a) => ReadS a
	
	lexDigits,        -- :: ReadS String

	-- * Miscellaneous

        fromRat,          -- :: (RealFloat a) => Rational -> a

	) where












import Data.Char



import Hugs.Prelude
import Hugs.Numeric






















































































































-- ---------------------------------------------------------------------------
-- Integer printing functions

-- | Shows a /non-negative/ 'Integral' number using the base specified by the
-- first argument, and the character representation specified by the second.
showIntAtBase :: Integral a => a -> (Int -> Char) -> a -> ShowS
showIntAtBase base toChr n r
  | base <= 1 = error ("Numeric.showIntAtBase: applied to unsupported base " ++ show base)
  | n <  0    = error ("Numeric.showIntAtBase: applied to negative number " ++ show n)
  | otherwise = showIt (quotRem n base) r
   where
    showIt (n,d) r = seq c $ -- stricter than necessary
      case n of
        0 -> r'
	_ -> showIt (quotRem n base) r'
     where
      c  = toChr (fromIntegral d) 
      r' = c : r

-- | Show /non-negative/ 'Integral' numbers in base 16.
showHex :: Integral a => a -> ShowS
showHex = showIntAtBase 16 intToDigit

-- | Show /non-negative/ 'Integral' numbers in base 8.
showOct :: Integral a => a -> ShowS
showOct = showIntAtBase 8  intToDigit
