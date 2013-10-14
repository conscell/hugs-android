{-# OPTIONS_GHC -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.GetOpt
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This library provides facilities for parsing the command-line options
-- in a standalone program.  It is essentially a Haskell port of the GNU 
-- @getopt@ library.
--
-----------------------------------------------------------------------------

{-
Sven Panne <Sven.Panne@informatik.uni-muenchen.de> Oct. 1996 (small
changes Dec. 1997)

Two rather obscure features are missing: The Bash 2.0 non-option hack
(if you don't already know it, you probably don't want to hear about
it...) and the recognition of long options with a single dash
(e.g. '-help' is recognised as '--help', as long as there is no short
option 'h').

Other differences between GNU's getopt and this implementation:

* To enforce a coherent description of options and arguments, there
  are explanation fields in the option/argument descriptor.

* Error messages are now more informative, but no longer POSIX
  compliant... :-(

And a final Haskell advertisement: The GNU C implementation uses well
over 1100 lines, we need only 195 here, including a 46 line example! 
:-)
-}

-- #hide
module Distribution.GetOpt (
   -- * GetOpt
   getOpt, getOpt',
   usageInfo,
   ArgOrder(..),
   OptDescr(..),
   ArgDescr(..),

   -- * Example

   -- $example
) where



import System.Console.GetOpt








































































































































































































































{- $example

To hopefully illuminate the role of the different data
structures, here\'s the command-line options for a (very simple)
compiler:

>    module Opts where
>    
>    import Distribution.GetOpt
>    import Data.Maybe ( fromMaybe )
>    
>    data Flag 
>     = Verbose  | Version 
>     | Input String | Output String | LibDir String
>       deriving Show
>    
>    options :: [OptDescr Flag]
>    options =
>     [ Option ['v']     ["verbose"] (NoArg Verbose)       "chatty output on stderr"
>     , Option ['V','?'] ["version"] (NoArg Version)       "show version number"
>     , Option ['o']     ["output"]  (OptArg outp "FILE")  "output FILE"
>     , Option ['c']     []          (OptArg inp  "FILE")  "input FILE"
>     , Option ['L']     ["libdir"]  (ReqArg LibDir "DIR") "library directory"
>     ]
>    
>    inp,outp :: Maybe String -> Flag
>    outp = Output . fromMaybe "stdout"
>    inp  = Input  . fromMaybe "stdin"
>    
>    compilerOpts :: [String] -> IO ([Flag], [String])
>    compilerOpts argv = 
>       case getOpt Permute options argv of
>          (o,n,[]  ) -> return (o,n)
>          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
>      where header = "Usage: ic [OPTION...] files..."

-}
