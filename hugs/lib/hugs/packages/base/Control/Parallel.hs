-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Parallel
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Parallel Constructs
--
-----------------------------------------------------------------------------

module Control.Parallel (
          par, seq -- re-exported



    ) where

import Prelude

































-- Maybe parIO and the like could be added here later.

-- | Indicates that it may be beneficial to evaluate the first
-- argument in parallel with the second.  Returns the value of the
-- second argument.
-- 
-- @a `par` b@ is exactly equivalent semantically to @b@.
--
-- @par@ is generally used when the value of @a@ is likely to be
-- required later, but not immediately.  Also it is a good idea to
-- ensure that @a@ is not a trivial computation, otherwise the cost of
-- spawning it in parallel overshadows the benefits obtained by
-- running it in parallel.
--
-- Note that actual parallelism is only supported by certain
-- implementations (GHC with the @-threaded@ option, and GPH, for
-- now).  On other implementations, @par a b = b@.
--
par :: a -> b -> b



-- For now, Hugs does not support par properly.
par a b = b

