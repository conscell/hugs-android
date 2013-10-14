{-# OPTIONS_GHC -fno-bang-patterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Base
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs, uses Control.Monad.ST)
--
-- Basis for IArray and MArray.  Not intended for external consumption;
-- use IArray or MArray instead.
--
-----------------------------------------------------------------------------

-- #hide
module Data.Array.Base where

import Prelude

import Control.Monad.ST.Lazy ( strictToLazyST )
import qualified Control.Monad.ST.Lazy as Lazy (ST)
import Data.Ix		( Ix, range, index, rangeSize )
import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.StablePtr
















import Data.Bits
import Foreign.Storable
import qualified Hugs.Array as Arr
import qualified Hugs.ST as ArrST
import Hugs.Array ( unsafeIndex )
import Hugs.ST ( STArray, ST(..), runST )
import Hugs.ByteArray


import Data.Typeable
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
























































                                                                                                                                                                                                        

                                                                        
                                                                          

                               
                                                                                                            







                                                      












                                                         












                                


                                  


                                 


                               


                                


                                     


                                 


                                         


                                        


                                         


                                              


                                          


                                  


                                                           


                                                         


                                                         


                                                         


                                                         


                                                          


                                                         


                                                           


                                                            


                                                         


                                                                              


                                              


                                                          


                                                          


                                            


                                                


                                                  


                                                 


                                               


                                                


                                                     


                                                 


                                                         


                                                        


                                                         


                                                              


                                                          


                                                  


                                                      




















































-----------------------------------------------------------------------------
-- Class of immutable arrays

{- | Class of immutable array types.

An array type has the form @(a i e)@ where @a@ is the array type
constructor (kind @* -> * -> *@), @i@ is the index type (a member of
the class 'Ix'), and @e@ is the element type.  The @IArray@ class is
parameterised over both @a@ and @e@, so that instances specialised to
certain element types can be defined.
-}
class IArray a e where
    -- | Extracts the bounds of an immutable array
    bounds           :: Ix i => a i e -> (i,i)
    unsafeArray      :: Ix i => (i,i) -> [(Int, e)] -> a i e
    unsafeAt         :: Ix i => a i e -> Int -> e
    unsafeReplace    :: Ix i => a i e -> [(Int, e)] -> a i e
    unsafeAccum      :: Ix i => (e -> e' -> e) -> a i e -> [(Int, e')] -> a i e
    unsafeAccumArray :: Ix i => (e -> e' -> e) -> e -> (i,i) -> [(Int, e')] -> a i e

    unsafeReplace arr ies = runST (unsafeReplaceST arr ies >>= unsafeFreeze)
    unsafeAccum f arr ies = runST (unsafeAccumST f arr ies >>= unsafeFreeze)
    unsafeAccumArray f e lu ies = runST (unsafeAccumArrayST f e lu ies >>= unsafeFreeze)

{-# INLINE unsafeReplaceST #-}
unsafeReplaceST :: (IArray a e, Ix i) => a i e -> [(Int, e)] -> ST s (STArray s i e)
unsafeReplaceST arr ies = do
    marr <- thaw arr
    sequence_ [unsafeWrite marr i e | (i, e) <- ies]
    return marr

{-# INLINE unsafeAccumST #-}
unsafeAccumST :: (IArray a e, Ix i) => (e -> e' -> e) -> a i e -> [(Int, e')] -> ST s (STArray s i e)
unsafeAccumST f arr ies = do
    marr <- thaw arr
    sequence_ [do
        old <- unsafeRead marr i
        unsafeWrite marr i (f old new)
        | (i, new) <- ies]
    return marr

{-# INLINE unsafeAccumArrayST #-}
unsafeAccumArrayST :: Ix i => (e -> e' -> e) -> e -> (i,i) -> [(Int, e')] -> ST s (STArray s i e)
unsafeAccumArrayST f e (l,u) ies = do
    marr <- newArray (l,u) e
    sequence_ [do
        old <- unsafeRead marr i
        unsafeWrite marr i (f old new)
        | (i, new) <- ies]
    return marr


{-# INLINE array #-} 

{-| Constructs an immutable array from a pair of bounds and a list of
initial associations.

The bounds are specified as a pair of the lowest and highest bounds in
the array respectively.  For example, a one-origin vector of length 10
has bounds (1,10), and a one-origin 10 by 10 matrix has bounds
((1,1),(10,10)).

An association is a pair of the form @(i,x)@, which defines the value of
the array at index @i@ to be @x@.  The array is undefined if any index
in the list is out of bounds.  If any two associations in the list have
the same index, the value at that index is implementation-dependent.
(In GHC, the last value specified for that index is used.
Other implementations will also do this for unboxed arrays, but Haskell
98 requires that for 'Array' the value at such indices is bottom.)

Because the indices must be checked for these errors, 'array' is
strict in the bounds argument and in the indices of the association
list.  Whether @array@ is strict or non-strict in the elements depends
on the array type: 'Data.Array.Array' is a non-strict array type, but
all of the 'Data.Array.Unboxed.UArray' arrays are strict.  Thus in a
non-strict array, recurrences such as the following are possible:

> a = array (1,100) ((1,1) : [(i, i * a!(i-1)) | i \<- [2..100]])

Not every index within the bounds of the array need appear in the
association list, but the values associated with indices that do not
appear will be undefined.

If, in any dimension, the lower bound is greater than the upper bound,
then the array is legal, but empty. Indexing an empty array always
gives an array-bounds error, but 'bounds' still yields the bounds with
which the array was constructed.
-}
array 	:: (IArray a e, Ix i) 
	=> (i,i)	-- ^ bounds of the array: (lowest,highest)
	-> [(i, e)]	-- ^ list of associations
	-> a i e
array (l,u) ies = unsafeArray (l,u) [(index (l,u) i, e) | (i, e) <- ies]

-- Since unsafeFreeze is not guaranteed to be only a cast, we will
-- use unsafeArray and zip instead of a specialized loop to implement
-- listArray, unlike Array.listArray, even though it generates some
-- unnecessary heap allocation. Will use the loop only when we have
-- fast unsafeFreeze, namely for Array and UArray (well, they cover
-- almost all cases).

{-# INLINE listArray #-}

-- | Constructs an immutable array from a list of initial elements.
-- The list gives the elements of the array in ascending order
-- beginning with the lowest index.
listArray :: (IArray a e, Ix i) => (i,i) -> [e] -> a i e
listArray (l,u) es = unsafeArray (l,u) (zip [0 .. rangeSize (l,u) - 1] es)

{-# INLINE listArrayST #-}
listArrayST :: Ix i => (i,i) -> [e] -> ST s (STArray s i e)
listArrayST (l,u) es = do
    marr <- newArray_ (l,u)
    let n = rangeSize (l,u)
    let fillFromList i xs | i == n    = return ()
                          | otherwise = case xs of
            []   -> return ()
            y:ys -> unsafeWrite marr i y >> fillFromList (i+1) ys
    fillFromList 0 es
    return marr

{-# RULES
"listArray/Array" listArray =
    \lu es -> runST (listArrayST lu es >>= ArrST.unsafeFreezeSTArray)
    #-}

{-# INLINE listUArrayST #-}
listUArrayST :: (MArray (STUArray s) e (ST s), Ix i)
             => (i,i) -> [e] -> ST s (STUArray s i e)
listUArrayST (l,u) es = do
    marr <- newArray_ (l,u)
    let n = rangeSize (l,u)
    let fillFromList i xs | i == n    = return ()
                          | otherwise = case xs of
            []   -> return ()
            y:ys -> unsafeWrite marr i y >> fillFromList (i+1) ys
    fillFromList 0 es
    return marr

-- I don't know how to write a single rule for listUArrayST, because
-- the type looks like constrained over 's', which runST doesn't
-- like. In fact all MArray (STUArray s) instances are polymorphic
-- wrt. 's', but runST can't know that.
--
-- More precisely, we'd like to write this:
--   listUArray :: (forall s. MArray (STUArray s) e (ST s), Ix i)
--	        => (i,i) -> [e] -> UArray i e
--   listUArray lu = runST (listUArrayST lu es >>= unsafeFreezeSTUArray)
--   {-# RULES listArray = listUArray
-- Then we could call listUArray at any type 'e' that had a suitable
-- MArray instance.  But sadly we can't, because we don't have quantified 
-- constraints.  Hence the mass of rules below.

-- I would like also to write a rule for listUArrayST (or listArray or
-- whatever) applied to unpackCString#. Unfortunately unpackCString#
-- calls seem to be floated out, then floated back into the middle
-- of listUArrayST, so I was not able to do this.










































{-# INLINE (!) #-}
-- | Returns the element of an immutable array at the specified index.
(!) :: (IArray a e, Ix i) => a i e -> i -> e
arr ! i = case bounds arr of (l,u) -> unsafeAt arr (index (l,u) i)

{-# INLINE indices #-}
-- | Returns a list of all the valid indices in an array.
indices :: (IArray a e, Ix i) => a i e -> [i]
indices arr = case bounds arr of (l,u) -> range (l,u)

{-# INLINE elems #-}
-- | Returns a list of all the elements of an array, in the same order
-- as their indices.
elems :: (IArray a e, Ix i) => a i e -> [e]
elems arr = case bounds arr of
    (l,u) -> [unsafeAt arr i | i <- [0 .. rangeSize (l,u) - 1]]

{-# INLINE assocs #-}
-- | Returns the contents of an array as a list of associations.
assocs :: (IArray a e, Ix i) => a i e -> [(i, e)]
assocs arr = case bounds arr of
    (l,u) -> [(i, unsafeAt arr (unsafeIndex (l,u) i)) | i <- range (l,u)]

{-# INLINE accumArray #-}

{-| 
Constructs an immutable array from a list of associations.  Unlike
'array', the same index is allowed to occur multiple times in the list
of associations; an /accumulating function/ is used to combine the
values of elements with the same index.

For example, given a list of values of some index type, hist produces
a histogram of the number of occurrences of each index within a
specified range:

> hist :: (Ix a, Num b) => (a,a) -> [a] -> Array a b
> hist bnds is = accumArray (+) 0 bnds [(i, 1) | i\<-is, inRange bnds i]
-}
accumArray :: (IArray a e, Ix i) 
	=> (e -> e' -> e) 	-- ^ An accumulating function
	-> e			-- ^ A default element
	-> (i,i)		-- ^ The bounds of the array
	-> [(i, e')]		-- ^ List of associations
	-> a i e		-- ^ Returns: the array
accumArray f init (l,u) ies =
    unsafeAccumArray f init (l,u) [(index (l,u) i, e) | (i, e) <- ies]

{-# INLINE (//) #-}
{-|
Takes an array and a list of pairs and returns an array identical to
the left argument except that it has been updated by the associations
in the right argument.  For example, if m is a 1-origin, n by n matrix,
then @m\/\/[((i,i), 0) | i \<- [1..n]]@ is the same matrix, except with
the diagonal zeroed.

As with the 'array' function, if any two associations in the list have
the same index, the value at that index is implementation-dependent.
(In GHC, the last value specified for that index is used.
Other implementations will also do this for unboxed arrays, but Haskell
98 requires that for 'Array' the value at such indices is bottom.)

For most array types, this operation is O(/n/) where /n/ is the size
of the array.  However, the 'Data.Array.Diff.DiffArray' type provides
this operation with complexity linear in the number of updates.
-}
(//) :: (IArray a e, Ix i) => a i e -> [(i, e)] -> a i e
arr // ies = case bounds arr of
    (l,u) -> unsafeReplace arr [(index (l,u) i, e) | (i, e) <- ies]

{-# INLINE accum #-}
{-|
@accum f@ takes an array and an association list and accumulates pairs
from the list into the array with the accumulating function @f@. Thus
'accumArray' can be defined using 'accum':

> accumArray f z b = accum f (array b [(i, z) | i \<- range b])
-}
accum :: (IArray a e, Ix i) => (e -> e' -> e) -> a i e -> [(i, e')] -> a i e
accum f arr ies = case bounds arr of
    (l,u) -> unsafeAccum f arr [(index (l,u) i, e) | (i, e) <- ies]

{-# INLINE amap #-}
-- | Returns a new array derived from the original array by applying a
-- function to each of the elements.
amap :: (IArray a e', IArray a e, Ix i) => (e' -> e) -> a i e' -> a i e
amap f arr = case bounds arr of
    (l,u) -> unsafeArray (l,u) [(i, f (unsafeAt arr i)) |
				i <- [0 .. rangeSize (l,u) - 1]]
{-# INLINE ixmap #-}
-- | Returns a new array derived from the original array by applying a
-- function to each of the indices.
ixmap :: (IArray a e, Ix i, Ix j) => (i,i) -> (i -> j) -> a j e -> a i e
ixmap (l,u) f arr =
    unsafeArray (l,u) [(unsafeIndex (l,u) i, arr ! f i) | i <- range (l,u)]

-----------------------------------------------------------------------------
-- Normal polymorphic arrays

instance IArray Arr.Array e where
    {-# INLINE bounds #-}
    bounds = Arr.bounds
    {-# INLINE unsafeArray #-}
    unsafeArray      = Arr.unsafeArray
    {-# INLINE unsafeAt #-}
    unsafeAt         = Arr.unsafeAt
    {-# INLINE unsafeReplace #-}
    unsafeReplace    = Arr.unsafeReplace
    {-# INLINE unsafeAccum #-}
    unsafeAccum      = Arr.unsafeAccum
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray = Arr.unsafeAccumArray

-----------------------------------------------------------------------------
-- Flat unboxed arrays

-- | Arrays with unboxed elements.  Instances of 'IArray' are provided
-- for 'UArray' with certain element types ('Int', 'Float', 'Char',
-- etc.; see the 'UArray' class for a full list).
--
-- A 'UArray' will generally be more efficient (in terms of both time
-- and space) than the equivalent 'Data.Array.Array' with the same
-- element type.  However, 'UArray' is strict in its elements - so
-- don\'t use 'UArray' if you require the non-strictness that
-- 'Data.Array.Array' provides.
--
-- Because the @IArray@ interface provides operations overloaded on
-- the type of the array, it should be possible to just change the
-- array type being used by a program from say @Array@ to @UArray@ to
-- get the benefits of unboxed arrays (don\'t forget to import
-- "Data.Array.Unboxed" instead of "Data.Array").
--




data UArray i e = UArray !i !i !ByteArray


uArrayTc = mkTyCon "UArray"; instance Typeable2 UArray where { typeOf2 _ = mkTyConApp uArrayTc [] }; instance Typeable a => Typeable1 (UArray a) where {   typeOf1 = typeOf1Default }; instance (Typeable a, Typeable b) => Typeable (UArray a b) where {   typeOf = typeOfDefault }

{-# INLINE unsafeArrayUArray #-}
unsafeArrayUArray :: (MArray (STUArray s) e (ST s), Ix i)
                  => (i,i) -> [(Int, e)] -> e -> ST s (UArray i e)
unsafeArrayUArray (l,u) ies default_elem = do
    marr <- newArray (l,u) default_elem
    sequence_ [unsafeWrite marr i e | (i, e) <- ies]
    unsafeFreezeSTUArray marr










unsafeFreezeSTUArray :: STUArray s i e -> ST s (UArray i e)
unsafeFreezeSTUArray (STUArray l u marr) = do
    arr <- unsafeFreezeMutableByteArray marr
    return (UArray l u arr)


{-# INLINE unsafeReplaceUArray #-}
unsafeReplaceUArray :: (MArray (STUArray s) e (ST s), Ix i)
                    => UArray i e -> [(Int, e)] -> ST s (UArray i e)
unsafeReplaceUArray arr ies = do
    marr <- thawSTUArray arr
    sequence_ [unsafeWrite marr i e | (i, e) <- ies]
    unsafeFreezeSTUArray marr

{-# INLINE unsafeAccumUArray #-}
unsafeAccumUArray :: (MArray (STUArray s) e (ST s), Ix i)
                  => (e -> e' -> e) -> UArray i e -> [(Int, e')] -> ST s (UArray i e)
unsafeAccumUArray f arr ies = do
    marr <- thawSTUArray arr
    sequence_ [do
        old <- unsafeRead marr i
        unsafeWrite marr i (f old new)
        | (i, new) <- ies]
    unsafeFreezeSTUArray marr

{-# INLINE unsafeAccumArrayUArray #-}
unsafeAccumArrayUArray :: (MArray (STUArray s) e (ST s), Ix i)
                       => (e -> e' -> e) -> e -> (i,i) -> [(Int, e')] -> ST s (UArray i e)
unsafeAccumArrayUArray f init (l,u) ies = do
    marr <- newArray (l,u) init
    sequence_ [do
        old <- unsafeRead marr i
        unsafeWrite marr i (f old new)
        | (i, new) <- ies]
    unsafeFreezeSTUArray marr

{-# INLINE eqUArray #-}
eqUArray :: (IArray UArray e, Ix i, Eq e) => UArray i e -> UArray i e -> Bool
eqUArray arr1@(UArray l1 u1 _) arr2@(UArray l2 u2 _) =
    if rangeSize (l1,u1) == 0 then rangeSize (l2,u2) == 0 else
    l1 == l2 && u1 == u2 &&
    and [unsafeAt arr1 i == unsafeAt arr2 i | i <- [0 .. rangeSize (l1,u1) - 1]]

{-# INLINE cmpUArray #-}
cmpUArray :: (IArray UArray e, Ix i, Ord e) => UArray i e -> UArray i e -> Ordering
cmpUArray arr1 arr2 = compare (assocs arr1) (assocs arr2)

{-# INLINE cmpIntUArray #-}
cmpIntUArray :: (IArray UArray e, Ord e) => UArray Int e -> UArray Int e -> Ordering
cmpIntUArray arr1@(UArray l1 u1 _) arr2@(UArray l2 u2 _) =
    if rangeSize (l1,u1) == 0 then if rangeSize (l2,u2) == 0 then EQ else LT else
    if rangeSize (l2,u2) == 0 then GT else
    case compare l1 l2 of
        EQ    -> foldr cmp (compare u1 u2) [0 .. rangeSize (l1, min u1 u2) - 1]
        other -> other
    where
    cmp i rest = case compare (unsafeAt arr1 i) (unsafeAt arr2 i) of
        EQ    -> rest
        other -> other

{-# RULES "cmpUArray/Int" cmpUArray = cmpIntUArray #-}

-----------------------------------------------------------------------------
-- Showing IArrays

{-# SPECIALISE 
    showsIArray :: (IArray UArray e, Ix i, Show i, Show e) => 
		   Int -> UArray i e -> ShowS
  #-}

showsIArray :: (IArray a e, Ix i, Show i, Show e) => Int -> a i e -> ShowS
showsIArray p a =
    showParen (p > 9) $
    showString "array " .
    shows (bounds a) .
    showChar ' ' .
    shows (assocs a)

-----------------------------------------------------------------------------
-- Flat unboxed arrays: instances


unsafeAtBArray :: Storable e => UArray i e -> Int -> e
unsafeAtBArray (UArray _ _ arr) = readByteArray arr


instance IArray UArray Bool where
    {-# INLINE bounds #-}
    bounds (UArray l u _) = (l,u)
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies False)







    unsafeAt (UArray _ _ arr) i =
	testBit (readByteArray arr (bOOL_INDEX i)::BitSet) (bOOL_SUBINDEX i)

    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Char where
    {-# INLINE bounds #-}
    bounds (UArray l u _) = (l,u)
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies '\0')
    {-# INLINE unsafeAt #-}




    unsafeAt = unsafeAtBArray

    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Int where
    {-# INLINE bounds #-}
    bounds (UArray l u _) = (l,u)
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies 0)





    unsafeAt = unsafeAtBArray

    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Word where
    {-# INLINE bounds #-}
    bounds (UArray l u _) = (l,u)
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies 0)





    unsafeAt = unsafeAtBArray

    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray (Ptr a) where
    {-# INLINE bounds #-}
    bounds (UArray l u _) = (l,u)
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies nullPtr)
    {-# INLINE unsafeAt #-}




    unsafeAt = unsafeAtBArray

    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray (FunPtr a) where
    {-# INLINE bounds #-}
    bounds (UArray l u _) = (l,u)
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies nullFunPtr)





    unsafeAt = unsafeAtBArray

    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Float where
    {-# INLINE bounds #-}
    bounds (UArray l u _) = (l,u)
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies 0)





    unsafeAt = unsafeAtBArray

    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Double where
    {-# INLINE bounds #-}
    bounds (UArray l u _) = (l,u)
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies 0)





    unsafeAt = unsafeAtBArray

    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray (StablePtr a) where
    {-# INLINE bounds #-}
    bounds (UArray l u _) = (l,u)
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies nullStablePtr)





    unsafeAt = unsafeAtBArray

    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

-- bogus StablePtr value for initialising a UArray of StablePtr.




nullStablePtr = castPtrToStablePtr nullPtr


instance IArray UArray Int8 where
    {-# INLINE bounds #-}
    bounds (UArray l u _) = (l,u)
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies 0)





    unsafeAt = unsafeAtBArray

    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Int16 where
    {-# INLINE bounds #-}
    bounds (UArray l u _) = (l,u)
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies 0)





    unsafeAt = unsafeAtBArray

    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Int32 where
    {-# INLINE bounds #-}
    bounds (UArray l u _) = (l,u)
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies 0)





    unsafeAt = unsafeAtBArray

    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Int64 where
    {-# INLINE bounds #-}
    bounds (UArray l u _) = (l,u)
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies 0)





    unsafeAt = unsafeAtBArray

    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Word8 where
    {-# INLINE bounds #-}
    bounds (UArray l u _) = (l,u)
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies 0)





    unsafeAt = unsafeAtBArray

    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Word16 where
    {-# INLINE bounds #-}
    bounds (UArray l u _) = (l,u)
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies 0)





    unsafeAt = unsafeAtBArray

    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Word32 where
    {-# INLINE bounds #-}
    bounds (UArray l u _) = (l,u)
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies 0)





    unsafeAt = unsafeAtBArray

    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance IArray UArray Word64 where
    {-# INLINE bounds #-}
    bounds (UArray l u _) = (l,u)
    {-# INLINE unsafeArray #-}
    unsafeArray lu ies = runST (unsafeArrayUArray lu ies 0)





    unsafeAt = unsafeAtBArray

    {-# INLINE unsafeReplace #-}
    unsafeReplace arr ies = runST (unsafeReplaceUArray arr ies)
    {-# INLINE unsafeAccum #-}
    unsafeAccum f arr ies = runST (unsafeAccumUArray f arr ies)
    {-# INLINE unsafeAccumArray #-}
    unsafeAccumArray f init lu ies = runST (unsafeAccumArrayUArray f init lu ies)

instance (Ix ix, Eq e, IArray UArray e) => Eq (UArray ix e) where
    (==) = eqUArray

instance (Ix ix, Ord e, IArray UArray e) => Ord (UArray ix e) where
    compare = cmpUArray

instance (Ix ix, Show ix, Show e, IArray UArray e) => Show (UArray ix e) where
    showsPrec = showsIArray

-----------------------------------------------------------------------------
-- Mutable arrays

{-# NOINLINE arrEleBottom #-}
arrEleBottom :: a
arrEleBottom = error "MArray: undefined array element"

{-| Class of mutable array types.

An array type has the form @(a i e)@ where @a@ is the array type
constructor (kind @* -> * -> *@), @i@ is the index type (a member of
the class 'Ix'), and @e@ is the element type.

The @MArray@ class is parameterised over both @a@ and @e@ (so that
instances specialised to certain element types can be defined, in the
same way as for 'IArray'), and also over the type of the monad, @m@,
in which the mutable array will be manipulated.
-}
class (Monad m) => MArray a e m where

    -- | Returns the bounds of the array
    getBounds   :: Ix i => a i e -> m (i,i)

    -- | Builds a new array, with every element initialised to the supplied 
    -- value.
    newArray    :: Ix i => (i,i) -> e -> m (a i e)

    -- | Builds a new array, with every element initialised to undefined.
    newArray_   :: Ix i => (i,i) -> m (a i e)

    unsafeRead  :: Ix i => a i e -> Int -> m e
    unsafeWrite :: Ix i => a i e -> Int -> e -> m ()

    {-# INLINE newArray #-}
	-- The INLINE is crucial, because until we know at least which monad 	
	-- we are in, the code below allocates like crazy.  So inline it,
	-- in the hope that the context will know the monad.
    newArray (l,u) init = do
        marr <- newArray_ (l,u)
        sequence_ [unsafeWrite marr i init | i <- [0 .. rangeSize (l,u) - 1]]
        return marr

    newArray_ (l,u) = newArray (l,u) arrEleBottom

    -- newArray takes an initialiser which all elements of
    -- the newly created array are initialised to.  newArray_ takes
    -- no initialiser, it is assumed that the array is initialised with
    -- "undefined" values.

    -- why not omit newArray_?  Because in the unboxed array case we would
    -- like to omit the initialisation altogether if possible.  We can't do
    -- this for boxed arrays, because the elements must all have valid values
    -- at all times in case of garbage collection.

    -- why not omit newArray?  Because in the boxed case, we can omit the
    -- default initialisation with undefined values if we *do* know the
    -- initial value and it is constant for all elements.

{-# INLINE newListArray #-}
-- | Constructs a mutable array from a list of initial elements.
-- The list gives the elements of the array in ascending order
-- beginning with the lowest index.
newListArray :: (MArray a e m, Ix i) => (i,i) -> [e] -> m (a i e)
newListArray (l,u) es = do
    marr <- newArray_ (l,u)
    let n = rangeSize (l,u)
    let fillFromList i xs | i == n    = return ()
                          | otherwise = case xs of
            []   -> return ()
            y:ys -> unsafeWrite marr i y >> fillFromList (i+1) ys
    fillFromList 0 es
    return marr

{-# INLINE readArray #-}
-- | Read an element from a mutable array
readArray :: (MArray a e m, Ix i) => a i e -> i -> m e
readArray marr i = do
  (l,u) <- getBounds marr
  unsafeRead marr (index (l,u) i)

{-# INLINE writeArray #-}
-- | Write an element in a mutable array
writeArray :: (MArray a e m, Ix i) => a i e -> i -> e -> m ()
writeArray marr i e = do
  (l,u) <- getBounds marr
  unsafeWrite marr (index (l,u) i) e

{-# INLINE getElems #-}
-- | Return a list of all the elements of a mutable array
getElems :: (MArray a e m, Ix i) => a i e -> m [e]
getElems marr = do 
  (l,u) <- getBounds marr
  sequence [unsafeRead marr i | i <- [0 .. rangeSize (l,u) - 1]]

{-# INLINE getAssocs #-}
-- | Return a list of all the associations of a mutable array, in
-- index order.
getAssocs :: (MArray a e m, Ix i) => a i e -> m [(i, e)]
getAssocs marr = do 
  (l,u) <- getBounds marr
  sequence [ do e <- unsafeRead marr (index (l,u) i); return (i,e)
           | i <- range (l,u)]

{-# INLINE mapArray #-}
-- | Constructs a new array derived from the original array by applying a
-- function to each of the elements.
mapArray :: (MArray a e' m, MArray a e m, Ix i) => (e' -> e) -> a i e' -> m (a i e)
mapArray f marr = do 
  (l,u) <- getBounds marr
  marr' <- newArray_ (l,u)
  sequence_ [do
        e <- unsafeRead marr i
        unsafeWrite marr' i (f e)
        | i <- [0 .. rangeSize (l,u) - 1]]
  return marr'

{-# INLINE mapIndices #-}
-- | Constructs a new array derived from the original array by applying a
-- function to each of the indices.
mapIndices :: (MArray a e m, Ix i, Ix j) => (i,i) -> (i -> j) -> a j e -> m (a i e)
mapIndices (l,u) f marr = do
    marr' <- newArray_ (l,u)
    sequence_ [do
        e <- readArray marr (f i)
        unsafeWrite marr' (unsafeIndex (l,u) i) e
        | i <- range (l,u)]
    return marr'

-----------------------------------------------------------------------------
-- Polymorphic non-strict mutable arrays (ST monad)

instance MArray (STArray s) e (ST s) where
    {-# INLINE getBounds #-}
    getBounds arr = return $! ArrST.boundsSTArray arr
    {-# INLINE newArray #-}
    newArray    = ArrST.newSTArray
    {-# INLINE unsafeRead #-}
    unsafeRead  = ArrST.unsafeReadSTArray
    {-# INLINE unsafeWrite #-}
    unsafeWrite = ArrST.unsafeWriteSTArray

instance MArray (STArray s) e (Lazy.ST s) where
    {-# INLINE getBounds #-}
    getBounds arr = strictToLazyST (return $! ArrST.boundsSTArray arr)
    {-# INLINE newArray #-}
    newArray (l,u) e    = strictToLazyST (ArrST.newSTArray (l,u) e)
    {-# INLINE unsafeRead #-}
    unsafeRead arr i    = strictToLazyST (ArrST.unsafeReadSTArray arr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite arr i e = strictToLazyST (ArrST.unsafeWriteSTArray arr i e)


sTArrayTc = mkTyCon "STArray"; instance Typeable3 STArray where { typeOf3 _ = mkTyConApp sTArrayTc [] }; instance Typeable a => Typeable2 (STArray a) where {   typeOf2 = typeOf2Default }; instance (Typeable a, Typeable b) => Typeable1 (STArray a b) where {   typeOf1 = typeOf1Default }; instance (Typeable a, Typeable b, Typeable c) => Typeable (STArray a b c) where {   typeOf = typeOfDefault }


-----------------------------------------------------------------------------
-- Flat unboxed mutable arrays (ST monad)

-- | A mutable array with unboxed elements, that can be manipulated in
-- the 'ST' monad.  The type arguments are as follows:
--
--  * @s@: the state variable argument for the 'ST' type
--
--  * @i@: the index type of the array (should be an instance of @Ix@)
--
--  * @e@: the element type of the array.  Only certain element types
--    are supported.
--
-- An 'STUArray' will generally be more efficient (in terms of both time
-- and space) than the equivalent boxed version ('STArray') with the same
-- element type.  However, 'STUArray' is strict in its elements - so
-- don\'t use 'STUArray' if you require the non-strictness that
-- 'STArray' provides.




data STUArray s i a = STUArray !i !i !(MutableByteArray s)


stUArrayTc = mkTyCon "STUArray"; instance Typeable3 STUArray where { typeOf3 _ = mkTyConApp stUArrayTc [] }; instance Typeable a => Typeable2 (STUArray a) where {   typeOf2 = typeOf2Default }; instance (Typeable a, Typeable b) => Typeable1 (STUArray a b) where {   typeOf1 = typeOf1Default }; instance (Typeable a, Typeable b, Typeable c) => Typeable (STUArray a b c) where {   typeOf = typeOfDefault }















































































































































































































































































































































newMBArray_ :: (Ix i, Storable e) => (i,i) -> ST s (STUArray s i e)
newMBArray_ = makeArray undefined
  where
    makeArray :: (Ix i, Storable e) => e -> (i,i) -> ST s (STUArray s i e)
    makeArray dummy (l,u) = do
	marr <- newMutableByteArray (rangeSize (l,u) * sizeOf dummy)
	return (STUArray l u marr)

unsafeReadMBArray :: Storable e => STUArray s i e -> Int -> ST s e
unsafeReadMBArray (STUArray _ _ marr) = readMutableByteArray marr

unsafeWriteMBArray :: Storable e => STUArray s i e -> Int -> e -> ST s ()
unsafeWriteMBArray (STUArray _ _ marr) = writeMutableByteArray marr

getBoundsMBArray (STUArray l u _) = return (l,u)

instance MArray (STUArray s) Bool (ST s) where
    getBounds = getBoundsMBArray
    newArray_ (l,u) = do
        marr <- newMutableByteArray (bOOL_SCALE (rangeSize (l,u)))
        return (STUArray l u marr)
    unsafeRead (STUArray _ _ marr) i = do
	let ix = bOOL_INDEX i
	    bit = bOOL_SUBINDEX i
	w <- readMutableByteArray marr ix
	return (testBit (w::BitSet) bit)
    unsafeWrite (STUArray _ _ marr) i e = do
	let ix = bOOL_INDEX i
	    bit = bOOL_SUBINDEX i
	w <- readMutableByteArray marr ix
	writeMutableByteArray marr ix
	    (if e then setBit (w::BitSet) bit else clearBit w bit)

instance MArray (STUArray s) Char (ST s) where
    getBounds = getBoundsMBArray
    newArray_ = newMBArray_
    unsafeRead = unsafeReadMBArray
    unsafeWrite = unsafeWriteMBArray

instance MArray (STUArray s) Int (ST s) where
    getBounds = getBoundsMBArray
    newArray_ = newMBArray_
    unsafeRead = unsafeReadMBArray
    unsafeWrite = unsafeWriteMBArray

instance MArray (STUArray s) Word (ST s) where
    getBounds = getBoundsMBArray
    newArray_ = newMBArray_
    unsafeRead = unsafeReadMBArray
    unsafeWrite = unsafeWriteMBArray

instance MArray (STUArray s) (Ptr a) (ST s) where
    getBounds = getBoundsMBArray
    newArray_ = newMBArray_
    unsafeRead = unsafeReadMBArray
    unsafeWrite = unsafeWriteMBArray

instance MArray (STUArray s) (FunPtr a) (ST s) where
    getBounds = getBoundsMBArray
    newArray_ = newMBArray_
    unsafeRead = unsafeReadMBArray
    unsafeWrite = unsafeWriteMBArray

instance MArray (STUArray s) Float (ST s) where
    getBounds = getBoundsMBArray
    newArray_ = newMBArray_
    unsafeRead = unsafeReadMBArray
    unsafeWrite = unsafeWriteMBArray

instance MArray (STUArray s) Double (ST s) where
    getBounds = getBoundsMBArray
    newArray_ = newMBArray_
    unsafeRead = unsafeReadMBArray
    unsafeWrite = unsafeWriteMBArray

instance MArray (STUArray s) (StablePtr a) (ST s) where
    getBounds = getBoundsMBArray
    newArray_ = newMBArray_
    unsafeRead = unsafeReadMBArray
    unsafeWrite = unsafeWriteMBArray

instance MArray (STUArray s) Int8 (ST s) where
    getBounds = getBoundsMBArray
    newArray_ = newMBArray_
    unsafeRead = unsafeReadMBArray
    unsafeWrite = unsafeWriteMBArray

instance MArray (STUArray s) Int16 (ST s) where
    getBounds = getBoundsMBArray
    newArray_ = newMBArray_
    unsafeRead = unsafeReadMBArray
    unsafeWrite = unsafeWriteMBArray

instance MArray (STUArray s) Int32 (ST s) where
    getBounds = getBoundsMBArray
    newArray_ = newMBArray_
    unsafeRead = unsafeReadMBArray
    unsafeWrite = unsafeWriteMBArray

instance MArray (STUArray s) Int64 (ST s) where
    getBounds = getBoundsMBArray
    newArray_ = newMBArray_
    unsafeRead = unsafeReadMBArray
    unsafeWrite = unsafeWriteMBArray

instance MArray (STUArray s) Word8 (ST s) where
    getBounds = getBoundsMBArray
    newArray_ = newMBArray_
    unsafeRead = unsafeReadMBArray
    unsafeWrite = unsafeWriteMBArray

instance MArray (STUArray s) Word16 (ST s) where
    getBounds = getBoundsMBArray
    newArray_ = newMBArray_
    unsafeRead = unsafeReadMBArray
    unsafeWrite = unsafeWriteMBArray

instance MArray (STUArray s) Word32 (ST s) where
    getBounds = getBoundsMBArray
    newArray_ = newMBArray_
    unsafeRead = unsafeReadMBArray
    unsafeWrite = unsafeWriteMBArray

instance MArray (STUArray s) Word64 (ST s) where
    getBounds = getBoundsMBArray
    newArray_ = newMBArray_
    unsafeRead = unsafeReadMBArray
    unsafeWrite = unsafeWriteMBArray

type BitSet = Word8

bitSetSize = bitSize (0::BitSet)

bOOL_SCALE :: Int -> Int
bOOL_SCALE n = (n + bitSetSize - 1) `div` bitSetSize
 
bOOL_INDEX :: Int -> Int
bOOL_INDEX i = i `div` bitSetSize

bOOL_SUBINDEX :: Int -> Int
bOOL_SUBINDEX i = i `mod` bitSetSize


-----------------------------------------------------------------------------
-- Freezing

-- | Converts a mutable array (any instance of 'MArray') to an
-- immutable array (any instance of 'IArray') by taking a complete
-- copy of it.
freeze :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
freeze marr = do
  (l,u) <- getBounds marr
  ies <- sequence [do e <- unsafeRead marr i; return (i,e)
                   | i <- [0 .. rangeSize (l,u) - 1]]
  return (unsafeArray (l,u) ies)
















-- In-place conversion of mutable arrays to immutable ones places
-- a proof obligation on the user: no other parts of your code can
-- have a reference to the array at the point where you unsafely
-- freeze it (and, subsequently mutate it, I suspect).

{- |
   Converts an mutable array into an immutable array.  The 
   implementation may either simply cast the array from
   one type to the other without copying the array, or it
   may take a full copy of the array.

   Note that because the array is possibly not copied, any subsequent
   modifications made to the mutable version of the array may be
   shared with the immutable version.  It is safe to use, therefore, if
   the mutable version is never modified after the freeze operation.

   The non-copying implementation is supported between certain pairs
   of array types only; one constraint is that the array types must
   have identical representations.  In GHC, The following pairs of
   array types have a non-copying O(1) implementation of
   'unsafeFreeze'.  Because the optimised versions are enabled by
   specialisations, you will need to compile with optimisation (-O) to
   get them.

     * 'Data.Array.IO.IOUArray' -> 'Data.Array.Unboxed.UArray'

     * 'Data.Array.ST.STUArray' -> 'Data.Array.Unboxed.UArray'

     * 'Data.Array.IO.IOArray' -> 'Data.Array.Array'

     * 'Data.Array.ST.STArray' -> 'Data.Array.Array'
-}
{-# INLINE unsafeFreeze #-}
unsafeFreeze :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
unsafeFreeze = freeze

{-# RULES
"unsafeFreeze/STArray"  unsafeFreeze = ArrST.unsafeFreezeSTArray
"unsafeFreeze/STUArray" unsafeFreeze = unsafeFreezeSTUArray
    #-}

-----------------------------------------------------------------------------
-- Thawing

-- | Converts an immutable array (any instance of 'IArray') into a
-- mutable array (any instance of 'MArray') by taking a complete copy
-- of it.
thaw :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
thaw arr = case bounds arr of
  (l,u) -> do
    marr <- newArray_ (l,u)
    sequence_ [unsafeWrite marr i (unsafeAt arr i)
               | i <- [0 .. rangeSize (l,u) - 1]]
    return marr



















thawSTUArray :: Ix i => UArray i e -> ST s (STUArray s i e)
thawSTUArray (UArray l u arr) = do
    marr <- thawByteArray arr
    return (STUArray l u marr)


-- In-place conversion of immutable arrays to mutable ones places
-- a proof obligation on the user: no other parts of your code can
-- have a reference to the array at the point where you unsafely
-- thaw it (and, subsequently mutate it, I suspect).

{- |
   Converts an immutable array into a mutable array.  The 
   implementation may either simply cast the array from
   one type to the other without copying the array, or it
   may take a full copy of the array.  

   Note that because the array is possibly not copied, any subsequent
   modifications made to the mutable version of the array may be
   shared with the immutable version.  It is only safe to use,
   therefore, if the immutable array is never referenced again in this
   thread, and there is no possibility that it can be also referenced
   in another thread.  If you use an unsafeThaw/write/unsafeFreeze
   sequence in a multi-threaded setting, then you must ensure that
   this sequence is atomic with respect to other threads, or a garbage
   collector crash may result (because the write may be writing to a
   frozen array).

   The non-copying implementation is supported between certain pairs
   of array types only; one constraint is that the array types must
   have identical representations.  In GHC, The following pairs of
   array types have a non-copying O(1) implementation of
   'unsafeThaw'.  Because the optimised versions are enabled by
   specialisations, you will need to compile with optimisation (-O) to
   get them.

     * 'Data.Array.Unboxed.UArray' -> 'Data.Array.IO.IOUArray'

     * 'Data.Array.Unboxed.UArray' -> 'Data.Array.ST.STUArray'

     * 'Data.Array.Array'  -> 'Data.Array.IO.IOArray'

     * 'Data.Array.Array'  -> 'Data.Array.ST.STArray'
-}
{-# INLINE unsafeThaw #-}
unsafeThaw :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
unsafeThaw = thaw













-- | Casts an 'STUArray' with one element type into one with a
-- different element type.  All the elements of the resulting array
-- are undefined (unless you know what you\'re doing...).







castSTUArray :: STUArray s ix a -> ST s (STUArray s ix b)
castSTUArray (STUArray l u marr) = return (STUArray l u marr)

