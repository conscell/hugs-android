-----------------------------------------------------------------------------
-- |
-- Module      :  System.Directory
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- System-independent interface to directory manipulation.
--
-----------------------------------------------------------------------------

module System.Directory 
   ( 
    -- $intro

    -- * Actions on directories
      createDirectory		-- :: FilePath -> IO ()
    , createDirectoryIfMissing  -- :: Bool -> FilePath -> IO ()
    , removeDirectory		-- :: FilePath -> IO ()
    , removeDirectoryRecursive  -- :: FilePath -> IO ()
    , renameDirectory		-- :: FilePath -> FilePath -> IO ()

    , getDirectoryContents      -- :: FilePath -> IO [FilePath]
    , getCurrentDirectory       -- :: IO FilePath
    , setCurrentDirectory       -- :: FilePath -> IO ()

    -- * Pre-defined directories
    , getHomeDirectory
    , getAppUserDataDirectory
    , getUserDocumentsDirectory
    , getTemporaryDirectory

    -- * Actions on files
    , removeFile		-- :: FilePath -> IO ()
    , renameFile                -- :: FilePath -> FilePath -> IO ()
    , copyFile                  -- :: FilePath -> FilePath -> IO ()
    
    , canonicalizePath
    , findExecutable

    -- * Existence tests
    , doesFileExist		-- :: FilePath -> IO Bool
    , doesDirectoryExist        -- :: FilePath -> IO Bool

    -- * Permissions

    -- $permissions

    , Permissions(
	Permissions,
	readable,		-- :: Permissions -> Bool
	writable,		-- :: Permissions -> Bool
	executable,		-- :: Permissions -> Bool
	searchable		-- :: Permissions -> Bool
      )

    , getPermissions            -- :: FilePath -> IO Permissions
    , setPermissions	        -- :: FilePath -> Permissions -> IO ()

    -- * Timestamps

    , getModificationTime       -- :: FilePath -> IO ClockTime
   ) where

import System.Directory.Internals
import System.Environment      ( getEnv )
import System.IO.Error
import Control.Monad           ( when, unless )







import Hugs.Directory



















































































































































































copyPermissions :: FilePath -> FilePath -> IO ()
copyPermissions fromFPath toFPath
  = getPermissions fromFPath >>= setPermissions toFPath



-- | @'createDirectoryIfMissing' parents dir@ creates a new directory 
-- @dir@ if it doesn\'t exist. If the first argument is 'True'
-- the function will also create all parent directories if they are missing.
createDirectoryIfMissing :: Bool     -- ^ Create its parents too?
		         -> FilePath -- ^ The path to the directory you want to make
		         -> IO ()
createDirectoryIfMissing parents file = do
  b <- doesDirectoryExist file
  case (b,parents, file) of 
    (_,     _, "") -> return ()
    (True,  _,  _) -> return ()
    (_,  True,  _) -> mapM_ (createDirectoryIfMissing False) (tail (pathParents file))
    (_, False,  _) -> createDirectory file


















































-- | @'removeDirectoryRecursive' dir@  removes an existing directory /dir/
-- together with its content and all subdirectories. Be careful, 
-- if the directory contains symlinks, the function will follow them.
removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive startLoc = do
  cont <- getDirectoryContents startLoc
  sequence_ [rm (startLoc `joinFileName` x) | x <- cont, x /= "." && x /= ".."]
  removeDirectory startLoc
  where
    rm :: FilePath -> IO ()
    rm f = do temp <- try (removeFile f)
              case temp of
                Left e  -> do isDir <- doesDirectoryExist f
                              -- If f is not a directory, re-throw the error
                              unless isDir $ ioError e
                              removeDirectoryRecursive f
                Right _ -> return ()



































































































































































{- |@'copyFile' old new@ copies the existing file from /old/ to /new/.
If the /new/ file already exists, it is atomically replaced by the /old/ file.
Neither path may refer to an existing directory.  The permissions of /old/ are
copied to /new/, if possible.
-}

{- NOTES:

It's tempting to try to remove the target file before opening it for
writing.  This could be useful: for example if the target file is an
executable that is in use, writing will fail, but unlinking first
would succeed.

However, it certainly isn't always what you want.

 * if the target file is hardlinked, removing it would break
   the hard link, but just opening would preserve it.

 * opening and truncating will preserve permissions and
   ACLs on the target.

 * If the destination file is read-only in a writable directory,
   we might want copyFile to fail.  Removing the target first
   would succeed, however.

 * If the destination file is special (eg. /dev/null), removing
   it is probably not the right thing.  Copying to /dev/null
   should leave /dev/null intact, not replace it with a plain
   file.

 * There's a small race condition between removing the target and
   opening it for writing during which time someone might
   create it again.
-}
copyFile :: FilePath -> FilePath -> IO ()
copyFile fromFPath toFPath =

	do readFile fromFPath >>= writeFile toFPath
	   try (copyPermissions fromFPath toFPath)
	   return ()





















































-- dummy implementation
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath fpath = return fpath


-- | Given an executable file name, searches for such file
-- in the directories listed in system PATH. The returned value 
-- is the path to the found executable or Nothing if there isn't
-- such executable. For example (findExecutable \"ghc\")
-- gives you the path to GHC.
findExecutable :: String -> IO (Maybe FilePath)
findExecutable binary =
























 do
  path <- getEnv "PATH"
  search (parseSearchPath path)
  where
    fileName = binary `joinFileExt` exeExtension

    search :: [FilePath] -> IO (Maybe FilePath)
    search [] = return Nothing
    search (d:ds) = do
	let path = d `joinFileName` fileName
	b <- doesFileExist path
	if b then return (Just path)
             else search ds















































































































































































































































{- | Returns the current user's home directory.

The directory returned is expected to be writable by the current user,
but note that it isn't generally considered good practice to store
application-specific data here; use 'getAppUserDataDirectory'
instead.

On Unix, 'getHomeDirectory' returns the value of the @HOME@
environment variable.  On Windows, the system is queried for a
suitable path; a typical path might be 
@C:/Documents And Settings/user@.

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of home directory.

* 'isDoesNotExistError'
The home directory for the current user does not exist, or
cannot be found.
-}
getHomeDirectory :: IO FilePath
getHomeDirectory =










  getEnv "HOME"


{- | Returns the pathname of a directory in which application-specific
data for the current user can be stored.  The result of
'getAppUserDataDirectory' for a given application is specific to
the current user.

The argument should be the name of the application, which will be used
to construct the pathname (so avoid using unusual characters that
might result in an invalid pathname).

Note: the directory may not actually exist, and may need to be created
first.  It is expected that the parent directory exists and is
writable.

On Unix, this function returns @$HOME\/.appName@.  On Windows, a
typical path might be 

> C:/Documents And Settings/user/Application Data/appName

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of application-specific data directory.

* 'isDoesNotExistError'
The home directory for the current user does not exist, or
cannot be found.
-}
getAppUserDataDirectory :: String -> IO FilePath
getAppUserDataDirectory appName = do







  path <- getEnv "HOME"
  return (path++'/':'.':appName)


{- | Returns the current user's document directory.

The directory returned is expected to be writable by the current user,
but note that it isn't generally considered good practice to store
application-specific data here; use 'getAppUserDataDirectory'
instead.

On Unix, 'getUserDocumentsDirectory' returns the value of the @HOME@
environment variable.  On Windows, the system is queried for a
suitable path; a typical path might be 
@C:\/Documents and Settings\/user\/My Documents@.

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of document directory.

* 'isDoesNotExistError'
The document directory for the current user does not exist, or
cannot be found.
-}
getUserDocumentsDirectory :: IO FilePath
getUserDocumentsDirectory = do






  getEnv "HOME"


{- | Returns the current directory for temporary files.

On Unix, 'getTemporaryDirectory' returns the value of the @TMPDIR@
environment variable or \"\/tmp\" if the variable isn\'t defined.
On Windows, the function checks for the existence of environment variables in 
the following order and uses the first path found:

* 
TMP environment variable. 

*
TEMP environment variable. 

*
USERPROFILE environment variable. 

*
The Windows directory

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of temporary directory.

The function doesn\'t verify whether the path exists.
-}
getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = do





  catch (getEnv "TMPDIR") (\ex -> return "/tmp")





















