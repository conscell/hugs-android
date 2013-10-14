{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Maybe
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The Maybe type, and associated operations.
--
-----------------------------------------------------------------------------

module Data.Maybe
   (
     Maybe(Nothing,Just)-- instance of: Eq, Ord, Show, Read,
			--		Functor, Monad, MonadPlus

   , maybe		-- :: b -> (a -> b) -> Maybe a -> b

   , isJust		-- :: Maybe a -> Bool
   , isNothing		-- :: Maybe a -> Bool
   , fromJust		-- :: Maybe a -> a
   , fromMaybe		-- :: a -> Maybe a -> a
   , listToMaybe        -- :: [a] -> Maybe a
   , maybeToList	-- :: Maybe a -> [a]
   , catMaybes		-- :: [Maybe a] -> [a]
   , mapMaybe		-- :: (a -> Maybe b) -> [a] -> [b]
   ) where
































































-- | The 'isJust' function returns 'True' iff its argument is of the
-- form @Just _@.
isJust         :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

-- | The 'isNothing' function returns 'True' iff its argument is 'Nothing'.
isNothing         :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

-- | The 'fromJust' function extracts the element out of a 'Just' and
-- throws an error if its argument is 'Nothing'.
fromJust          :: Maybe a -> a
fromJust Nothing  = error "Maybe.fromJust: Nothing" -- yuck
fromJust (Just x) = x

-- | The 'fromMaybe' function takes a default value and and 'Maybe'
-- value.  If the 'Maybe' is 'Nothing', it returns the default values;
-- otherwise, it returns the value contained in the 'Maybe'.
fromMaybe     :: a -> Maybe a -> a
fromMaybe d x = case x of {Nothing -> d;Just v  -> v}

-- | The 'maybeToList' function returns an empty list when given
-- 'Nothing' or a singleton list when not given 'Nothing'.
maybeToList            :: Maybe a -> [a]
maybeToList  Nothing   = []
maybeToList  (Just x)  = [x]

-- | The 'listToMaybe' function returns 'Nothing' on an empty list
-- or @'Just' a@ where @a@ is the first element of the list.
listToMaybe           :: [a] -> Maybe a
listToMaybe []        =  Nothing
listToMaybe (a:_)     =  Just a

-- | The 'catMaybes' function takes a list of 'Maybe's and returns
-- a list of all the 'Just' values. 
catMaybes              :: [Maybe a] -> [a]
catMaybes ls = [x | Just x <- ls]

-- | The 'mapMaybe' function is a version of 'map' which can throw
-- out elements.  In particular, the functional argument returns
-- something of type @'Maybe' b@.  If this is 'Nothing', no element
-- is added on to the result list.  If it just @'Just' b@, then @b@ is
-- included in the result list.
mapMaybe          :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []     = []
mapMaybe f (x:xs) =
 let rs = mapMaybe f xs in
 case f x of
  Nothing -> rs
  Just r  -> r:rs


