-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- A common interface to a collection of useful concurrency
-- abstractions.
--
-----------------------------------------------------------------------------

module Control.Concurrent (
	-- * Concurrent Haskell

	-- $conc_intro

	-- * Basic concurrency operations

        ThreadId,




	forkIO,





	-- * Scheduling

	-- $conc_scheduling	
	yield,         		-- :: IO ()

	-- ** Blocking
	
	-- $blocking








	-- * Communication abstractions

	module Control.Concurrent.MVar,
	module Control.Concurrent.Chan,
	module Control.Concurrent.QSem,
	module Control.Concurrent.QSemN,
	module Control.Concurrent.SampleVar,

	-- * Merging of streams




	-- $merge











	-- * GHC's implementation of concurrency

	-- |This section describes features specific to GHC's
	-- implementation of Concurrent Haskell.
	
	-- ** Haskell threads and Operating System threads

	-- $osthreads

	-- ** Terminating the program

	-- $termination

	-- ** Pre-emption

	-- $preemption
    ) where

import Prelude

import Control.Exception as Exception

















import Hugs.ConcBase


import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent.QSem
import Control.Concurrent.QSemN
import Control.Concurrent.SampleVar


type ThreadId = ()


{- $conc_intro

The concurrency extension for Haskell is described in the paper
/Concurrent Haskell/
<http://www.haskell.org/ghc/docs/papers/concurrent-haskell.ps.gz>.

Concurrency is \"lightweight\", which means that both thread creation
and context switching overheads are extremely low.  Scheduling of
Haskell threads is done internally in the Haskell runtime system, and
doesn't make use of any operating system-supplied thread packages.

However, if you want to interact with a foreign library that expects your
program to use the operating system-supplied thread package, you can do so
by using 'forkOS' instead of 'forkIO'.

Haskell threads can communicate via 'MVar's, a kind of synchronised
mutable variable (see "Control.Concurrent.MVar").  Several common
concurrency abstractions can be built from 'MVar's, and these are
provided by the "Control.Concurrent" library.
In GHC, threads may also communicate via exceptions.
-}

{- $conc_scheduling

    Scheduling may be either pre-emptive or co-operative,
    depending on the implementation of Concurrent Haskell (see below
    for information related to specific compilers).  In a co-operative
    system, context switches only occur when you use one of the
    primitives defined in this module.  This means that programs such
    as:


>   main = forkIO (write 'a') >> write 'b'
>     where write c = putChar c >> write c

    will print either @aaaaaaaaaaaaaa...@ or @bbbbbbbbbbbb...@,
    instead of some random interleaving of @a@s and @b@s.  In
    practice, cooperative multitasking is sufficient for writing
    simple graphical user interfaces.  
-}

{- $blocking
Calling a foreign C procedure (such as @getchar@) that blocks waiting
for input will block /all/ threads, unless the @threadsafe@ attribute
is used on the foreign call (and your compiler \/ operating system
supports it).  GHC's I\/O system uses non-blocking I\/O internally to
implement thread-friendly I\/O, so calling standard Haskell I\/O
functions blocks only the thread making the call.
-}





































































































































































































































-- ---------------------------------------------------------------------------
-- More docs

{- $osthreads

      #osthreads# In GHC, threads created by 'forkIO' are lightweight threads, and
      are managed entirely by the GHC runtime.  Typically Haskell
      threads are an order of magnitude or two more efficient (in
      terms of both time and space) than operating system threads.

      The downside of having lightweight threads is that only one can
      run at a time, so if one thread blocks in a foreign call, for
      example, the other threads cannot continue.  The GHC runtime
      works around this by making use of full OS threads where
      necessary.  When the program is built with the @-threaded@
      option (to link against the multithreaded version of the
      runtime), a thread making a @safe@ foreign call will not block
      the other threads in the system; another OS thread will take
      over running Haskell threads until the original call returns.
      The runtime maintains a pool of these /worker/ threads so that
      multiple Haskell threads can be involved in external calls
      simultaneously.

      The "System.IO" library manages multiplexing in its own way.  On
      Windows systems it uses @safe@ foreign calls to ensure that
      threads doing I\/O operations don't block the whole runtime,
      whereas on Unix systems all the currently blocked I\/O reqwests
      are managed by a single thread (the /IO manager thread/) using
      @select@.

      The runtime will run a Haskell thread using any of the available
      worker OS threads.  If you need control over which particular OS
      thread is used to run a given Haskell thread, perhaps because
      you need to call a foreign library that uses OS-thread-local
      state, then you need "bound threads" (see above).

      If you don't use the @-threaded@ option, then the runtime does
      not make use of multiple OS threads.  Foreign calls will block
      all other running Haskell threads until the call returns.  The
      "System.IO" library still does multiplexing, so there can be multiple
      threads doing I\/O, and this is handled internally by the runtime using
      @select@.
-}

{- $termination

      In a standalone GHC program, only the main thread is
      required to terminate in order for the process to terminate.
      Thus all other forked threads will simply terminate at the same
      time as the main thread (the terminology for this kind of
      behaviour is \"daemonic threads\").

      If you want the program to wait for child threads to
      finish before exiting, you need to program this yourself.  A
      simple mechanism is to have each child thread write to an
      'MVar' when it completes, and have the main
      thread wait on all the 'MVar's before
      exiting:

>   myForkIO :: IO () -> IO (MVar ())
>   myForkIO io = do
>     mvar <- newEmptyMVar
>     forkIO (io `finally` putMVar mvar ())
>     return mvar

      Note that we use 'finally' from the
      "Control.Exception" module to make sure that the
      'MVar' is written to even if the thread dies or
      is killed for some reason.

      A better method is to keep a global list of all child
      threads which we should wait for at the end of the program:

>    children :: MVar [MVar ()]
>    children = unsafePerformIO (newMVar [])
>    
>    waitForChildren :: IO ()
>    waitForChildren = do
>      cs <- takeMVar children
>      case cs of
>        []   -> return ()
>        m:ms -> do
>    	    putMVar children ms
>    	    takeMVar m
>    	    waitForChildren
>    
>    forkChild :: IO () -> IO ()
>    forkChild io = do
>    	 mvar <- newEmptyMVar
>    	 childs <- takeMVar children
>    	 putMVar children (mvar:childs)
>    	 forkIO (io `finally` putMVar mvar ())
>
>     main =
>     	later waitForChildren $
>     	...

      The main thread principle also applies to calls to Haskell from
      outside, using @foreign export@.  When the @foreign export@ed
      function is invoked, it starts a new main thread, and it returns
      when this main thread terminates.  If the call causes new
      threads to be forked, they may remain in the system after the
      @foreign export@ed function has returned.
-}

{- $preemption

      GHC implements pre-emptive multitasking: the execution of
      threads are interleaved in a random fashion.  More specifically,
      a thread may be pre-empted whenever it allocates some memory,
      which unfortunately means that tight loops which do no
      allocation tend to lock out other threads (this only seems to
      happen with pathological benchmark-style code, however).

      The rescheduling timer runs on a 20ms granularity by
      default, but this may be altered using the
      @-i\<n\>@ RTS option.  After a rescheduling
      \"tick\" the running thread is pre-empted as soon as
      possible.

      One final note: the
      @aaaa@ @bbbb@ example may not
      work too well on GHC (see Scheduling, above), due
      to the locking on a 'System.IO.Handle'.  Only one thread
      may hold the lock on a 'System.IO.Handle' at any one
      time, so if a reschedule happens while a thread is holding the
      lock, the other thread won't be able to run.  The upshot is that
      the switch from @aaaa@ to
      @bbbbb@ happens infrequently.  It can be
      improved by lowering the reschedule tick period.  We also have a
      patch that causes a reschedule whenever a thread waiting on a
      lock is woken up, but haven't found it to be useful for anything
      other than this example :-)
-}
