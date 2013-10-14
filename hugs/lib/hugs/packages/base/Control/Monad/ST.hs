-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ST
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires universal quantification for runST)
--
-- This library provides support for /strict/ state threads, as
-- described in the PLDI \'94 paper by John Launchbury and Simon Peyton
-- Jones /Lazy Functional State Threads/.
--
-----------------------------------------------------------------------------

module Control.Monad.ST
  (
	-- * The 'ST' Monad
	ST,		-- abstract, instance of Functor, Monad, Typeable.
	runST,		-- :: (forall s. ST s a) -> a
	fixST,		-- :: (a -> ST s a) -> ST s a

	-- * Converting 'ST' to 'IO'
	RealWorld,		-- abstract
	stToIO,			-- :: ST RealWorld a -> IO a

	-- * Unsafe operations
	unsafeInterleaveST,  	-- :: ST s a -> ST s a
	unsafeIOToST,		-- :: IO a -> ST s a
	unsafeSTToIO		-- :: ST s a -> IO a
      ) where

import Prelude

import Control.Monad.Fix

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      

























































import Data.Typeable
import Hugs.ST
import qualified Hugs.LazyST as LazyST

sTTc = mkTyCon "ST"; instance Typeable2 ST where { typeOf2 _ = mkTyConApp sTTc [] }; instance Typeable a => Typeable1 (ST a) where {   typeOf1 = typeOf1Default }; instance (Typeable a, Typeable b) => Typeable (ST a b) where {   typeOf = typeOfDefault }
realWorldTc = mkTyCon "RealWorld"; instance Typeable RealWorld where { typeOf _ = mkTyConApp realWorldTc [] }

fixST :: (a -> ST s a) -> ST s a
fixST f = LazyST.lazyToStrictST (LazyST.fixST (LazyST.strictToLazyST . f))

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST =
    LazyST.lazyToStrictST . LazyST.unsafeInterleaveST . LazyST.strictToLazyST








instance MonadFix (ST s) where
	mfix = fixST

