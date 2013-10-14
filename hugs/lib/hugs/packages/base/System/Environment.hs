-----------------------------------------------------------------------------
-- |
-- Module      :  System.Environment
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Miscellaneous information about the system environment.
--
-----------------------------------------------------------------------------

module System.Environment
    ( 
      getArgs,	     -- :: IO [String]
      getProgName,   -- :: IO String
      getEnv,        -- :: String -> IO String

      withArgs,
      withProgName,




  ) where

import Prelude










import Hugs.System










-- ---------------------------------------------------------------------------
-- getArgs, getProgName, getEnv

-- | Computation 'getArgs' returns a list of the program's command
-- line arguments (not including the program name).












































































































































