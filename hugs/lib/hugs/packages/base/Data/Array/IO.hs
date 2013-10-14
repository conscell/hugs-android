{-# OPTIONS_GHC -#include "HsBase.h" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.IO
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Array.MArray)
--
-- Mutable boxed and unboxed arrays in the IO monad.
--
-----------------------------------------------------------------------------

module Data.Array.IO (
   -- * @IO@ arrays with boxed elements
   IOArray,		-- instance of: Eq, Typeable

   -- * @IO@ arrays with unboxed elements
   IOUArray,		-- instance of: Eq, Typeable
   castIOUArray,	-- :: IOUArray i a -> IO (IOUArray i b)

   -- * Overloaded mutable array interface
   module Data.Array.MArray,

   -- * Doing I\/O with @IOUArray@s
   hGetArray,		-- :: Handle -> IOUArray Int Word8 -> Int -> IO Int
   hPutArray,		-- :: Handle -> IOUArray Int Word8 -> Int -> IO ()
 ) where

import Prelude

import Data.Array.Base
import Data.Array.IO.Internals
import Data.Array		( Array )
import Data.Array.MArray
import Data.Int
import Data.Word









import Data.Char
import System.IO
import System.IO.Error
















































































































































































hGetArray :: Handle -> IOUArray Int Word8 -> Int -> IO Int
hGetArray handle arr count = do
	bds <- getBounds arr
	if count < 0 || count > rangeSize bds
	   then illegalBufferSize handle "hGetArray" count
	   else get 0
 where
  get i | i == count = return i
	| otherwise = do
		error_or_c <- try (hGetChar handle)
		case error_or_c of
		    Left ex
			| isEOFError ex -> return i
			| otherwise -> ioError ex
		    Right c -> do
			unsafeWrite arr i (fromIntegral (ord c))
			get (i+1)

hPutArray :: Handle -> IOUArray Int Word8 -> Int -> IO ()
hPutArray handle arr count = do
	bds <- getBounds arr
	if count < 0 || count > rangeSize bds
	   then illegalBufferSize handle "hPutArray" count
	   else put 0
 where
  put i | i == count = return ()
	| otherwise = do
		w <- unsafeRead arr i
		hPutChar handle (chr (fromIntegral w))
		put (i+1)

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize _ fn sz = ioError $
	userError (fn ++ ": illegal buffer size " ++ showsPrec 9 (sz::Int) [])

