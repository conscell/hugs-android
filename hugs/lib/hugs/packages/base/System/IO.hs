{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.IO
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The standard IO library.
--
-----------------------------------------------------------------------------

module System.IO (
    -- * The IO monad

    IO,			       -- instance MonadFix
    fixIO,		       -- :: (a -> IO a) -> IO a

    -- * Files and handles

    FilePath,		       -- :: String

    Handle,		-- abstract, instance of: Eq, Show.

    -- ** Standard handles

    -- | Three handles are allocated during program initialisation,
    -- and are initially open.

    stdin, stdout, stderr,   -- :: Handle

    -- * Opening and closing files

    -- ** Opening files

    openFile,		       -- :: FilePath -> IOMode -> IO Handle
    IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),

    -- ** Closing files

    hClose,		       -- :: Handle -> IO ()

    -- ** Special cases

    -- | These functions are also exported by the "Prelude".

    readFile,		       -- :: FilePath -> IO String
    writeFile,		       -- :: FilePath -> String -> IO ()
    appendFile,		       -- :: FilePath -> String -> IO ()

    -- ** File locking

    -- $locking

    -- * Operations on handles

    -- ** Determining and changing the size of a file

    hFileSize,		       -- :: Handle -> IO Integer




    -- ** Detecting the end of input

    hIsEOF,		       -- :: Handle -> IO Bool
    isEOF,		       -- :: IO Bool

    -- ** Buffering operations

    BufferMode(NoBuffering,LineBuffering,BlockBuffering),
    hSetBuffering,	       -- :: Handle -> BufferMode -> IO ()
    hGetBuffering,	       -- :: Handle -> IO BufferMode
    hFlush,		       -- :: Handle -> IO ()

    -- ** Repositioning handles

    hGetPosn,		       -- :: Handle -> IO HandlePosn
    hSetPosn,		       -- :: HandlePosn -> IO ()
    HandlePosn,                -- abstract, instance of: Eq, Show.

    hSeek,		       -- :: Handle -> SeekMode -> Integer -> IO ()
    SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),

    hTell,		       -- :: Handle -> IO Integer


    -- ** Handle properties

    hIsOpen, hIsClosed,        -- :: Handle -> IO Bool
    hIsReadable, hIsWritable,  -- :: Handle -> IO Bool
    hIsSeekable,               -- :: Handle -> IO Bool

    -- ** Terminal operations


    hIsTerminalDevice,	 	-- :: Handle -> IO Bool

    hSetEcho,			-- :: Handle -> Bool -> IO ()
    hGetEcho,			-- :: Handle -> IO Bool


    -- ** Showing handle state





    -- * Text input and output

    -- ** Text input

    hWaitForInput,	       -- :: Handle -> Int -> IO Bool
    hReady,		       -- :: Handle -> IO Bool
    hGetChar,		       -- :: Handle -> IO Char
    hGetLine,		       -- :: Handle -> IO [Char]
    hLookAhead,		       -- :: Handle -> IO Char
    hGetContents,	       -- :: Handle -> IO [Char]

    -- ** Text output

    hPutChar,		       -- :: Handle -> Char -> IO ()
    hPutStr,		       -- :: Handle -> [Char] -> IO ()
    hPutStrLn,		       -- :: Handle -> [Char] -> IO ()
    hPrint,		       -- :: Show a => Handle -> a -> IO ()

    -- ** Special cases for standard input and output

    -- | These functions are also exported by the "Prelude".

    interact,		       -- :: (String -> String) -> IO ()
    putChar,		       -- :: Char   -> IO ()
    putStr,		       -- :: String -> IO () 
    putStrLn,		       -- :: String -> IO ()
    print,		       -- :: Show a => a -> IO ()
    getChar,		       -- :: IO Char
    getLine,		       -- :: IO String
    getContents,	       -- :: IO String
    readIO,		       -- :: Read a => String -> IO a
    readLn,		       -- :: Read a => IO a

    -- * Binary input and output

    openBinaryFile,	       -- :: FilePath -> IOMode -> IO Handle
    hSetBinaryMode,	       -- :: Handle -> Bool -> IO ()

    hPutBuf,		       -- :: Handle -> Ptr a -> Int -> IO ()
    hGetBuf,		       -- :: Handle -> Ptr a -> Int -> IO Int






    -- * Temporary files





  ) where













import Hugs.IO
import Hugs.IOExts
import Hugs.IORef
import Hugs.Prelude	( throw, Exception(NonTermination) )
import System.IO.Unsafe	( unsafeInterleaveIO )









































-- -----------------------------------------------------------------------------
-- Standard IO












































































































-- | Computation 'hReady' @hdl@ indicates whether at least one item is
-- available for input from handle @hdl@.
-- 
-- This operation may fail with:
--
--  * 'System.IO.Error.isEOFError' if the end of file has been reached.

hReady		:: Handle -> IO Bool
hReady h 	=  hWaitForInput h 0

-- | The same as 'hPutStr', but adds a newline character.

hPutStrLn	:: Handle -> String -> IO ()
hPutStrLn hndl str = do
 hPutStr  hndl str
 hPutChar hndl '\n'

-- | Computation 'hPrint' @hdl t@ writes the string representation of @t@
-- given by the 'shows' function to the file or channel managed by @hdl@
-- and appends a newline.
--
-- This operation may fail with:
--
--  * 'System.IO.Error.isFullError' if the device is full; or
--
--  * 'System.IO.Error.isPermissionError' if another system resource limit would be exceeded.

hPrint		:: Show a => Handle -> a -> IO ()
hPrint hdl 	=  hPutStrLn hdl . show


-- ---------------------------------------------------------------------------
-- fixIO


fixIO :: (a -> IO a) -> IO a
fixIO k = do
    ref <- newIORef (throw NonTermination)
    ans <- unsafeInterleaveIO (readIORef ref)
    result <- k ans
    writeIORef ref result
    return result

-- NOTE: we do our own explicit black holing here, because GHC's lazy
-- blackholing isn't enough.  In an infinite loop, GHC may run the IO
-- computation a few times before it notices the loop, which is wrong.








-- $locking
-- Implementations should enforce as far as possible, at least locally to the
-- Haskell process, multiple-reader single-writer locking on files.
-- That is, /there may either be many handles on the same file which manage
-- input, or just one handle on the file which manages output/.  If any
-- open or semi-closed handle is managing a file for output, no new
-- handle can be allocated for that file.  If any open or semi-closed
-- handle is managing a file for input, new handles can only be allocated
-- if they do not manage output.  Whether two files are the same is
-- implementation-dependent, but they should normally be the same if they
-- have the same absolute path name and neither has been renamed, for
-- example.
--
-- /Warning/: the 'readFile' operation holds a semi-closed handle on
-- the file until the entire contents of the file have been consumed.
-- It follows that an attempt to write to a file (using 'writeFile', for
-- example) that was earlier opened by 'readFile' will usually result in
-- failure with 'System.IO.Error.isAlreadyInUseError'.

-- -----------------------------------------------------------------------------
-- Utils


















