{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.C.Types
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Mapping of C types to corresponding Haskell types.
--
-----------------------------------------------------------------------------

module Foreign.C.Types
	( -- * Representations of C types

	  -- $ctypes

	  -- ** Integral types
	  -- | These types are are represented as @newtype@s of
	  -- types in "Data.Int" and "Data.Word", and are instances of
	  -- 'Prelude.Eq', 'Prelude.Ord', 'Prelude.Num', 'Prelude.Read',
	  -- 'Prelude.Show', 'Prelude.Enum', 'Typeable', 'Storable',
	  -- 'Prelude.Bounded', 'Prelude.Real', 'Prelude.Integral' and
	  -- 'Bits'.
	  CChar,  CSChar,  CUChar
	, CShort, CUShort, CInt,   CUInt
	, CLong,  CULong
	, CPtrdiff, CSize, CWchar, CSigAtomic
        , CLLong, CULLong
	, CIntPtr, CUIntPtr
	, CIntMax, CUIntMax

	  -- ** Numeric types
	  -- | These types are are represented as @newtype@s of basic
	  -- foreign types, and are instances of
	  -- 'Prelude.Eq', 'Prelude.Ord', 'Prelude.Num', 'Prelude.Read',
	  -- 'Prelude.Show', 'Prelude.Enum', 'Typeable' and 'Storable'.
	, CClock,   CTime

	  -- ** Floating types
	  -- | These types are are represented as @newtype@s of
	  -- 'Prelude.Float' and 'Prelude.Double', and are instances of
	  -- 'Prelude.Eq', 'Prelude.Ord', 'Prelude.Num', 'Prelude.Read',
	  -- 'Prelude.Show', 'Prelude.Enum', 'Typeable', 'Storable',
	  -- 'Prelude.Real', 'Prelude.Fractional', 'Prelude.Floating',
	  -- 'Prelude.RealFrac' and 'Prelude.RealFloat'.
	, CFloat,  CDouble, CLDouble










	  -- ** Other types

          -- Instances of: Eq and Storable
	, CFile,        CFpos,     CJmpBuf
	) where



import Foreign.Storable
import Data.Bits	( Bits(..) )
import Data.Int		( Int8,  Int16,  Int32,  Int64  )
import Data.Word	( Word8, Word16, Word32, Word64 )
import Data.Typeable










import Control.Monad	( liftM )



import Hugs.Ptr		( castPtr )


                                                                              
                                                                             

                         


                          


                              


                                 


                        


                                


                          


                            


                         


                           


                           


                         


                          


                         


                                


                                


                              


                           


                                


                          


                        


                          


                          


                          


                         


                          


                             


                                


                         


                          


                               


                         


                          


                       


                           


                          


                         


                          


                          


                            


                             


                                


                            


                             


                               


                          


                           


                            


                           


                          


                          


                           


                          


                           


                          


                          


                          


                               


                          


                         


                          


                          


                           


                            


                           


                             


                            


                          


                         


                              


                         


                                


                         


                            


                                


                                 


                                


                          


                                   


                              


                          


                           


                           


                         


                                


                            


                             


                                   


                          


                         


                          


                          


                         


                             


                                


                           


                          


                               


                         


                            


                           


                           


                          


                           


                           


                          


                          


                          


                          


                           


                           


                           


                           


                           


                           


                           


                          


                           


                           


                           


                           


                           


                          


                           


                           


                             


                           


                           


                             


                           


                           


                           


                               


                               


                                                        


                                                                                     


                                                                                    
                             

                                                         


                                                        


                                                        


                                                  


                                                   


                                                       


                                                      
                          

                                                       


                                                          


                                                      


                                                           


                                                      


                                                         


                                                         


                                                         


                                                   


                                                         


                                                       


                                                       


                                                         


                                                         


                                                         


                                                          


                                                         


                                                        


                                                               


                                                             


                                                           


                                                              


                                                            


                                                             
                              

                                                            


                                                           


                                                            


                                                              


                                                           


                                                          


                                                   


                                                       


                                                                                                         


                                                                                         
                        

                                                         


                                                        


                                                   


                                                        
                         

                                                         


                                                          
                           

                                                          
                           

                                  


                                   


                                                     
                         

                                     


                                     


                                        


                                      


                                       


                                      


                                      


                                      


                                    


                                         


                                         


                                     


                                          


                                       


                                        


                                      


                                      


                                          


                                       


                                      


                                            


                                             


                                       


                                        


                                        


                                         


                                       


                                      


                                          


                                          


                                              


                                             


                                              


                                                   


                                               


                                        


                                       
                         

                                                                              


                                              


                                                          


                                                          


                                            


                                           


                                                      


                                                                           


                                                            
                           

                                                                       
                              

                                                 
                         

                                                            
                  

                                                       
                  

                                                
                  


                                                                                                                                                                                                                                              




                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
























































                                                                                                                                                                                                                                        

                                                                





































































































































































































-- | Haskell type representing the C @char@ type.
newtype CChar = CChar Int8 deriving (Eq, Ord) ; instance Num CChar where {    (CChar i) + (CChar j) = CChar (i + j) ;    (CChar i) - (CChar j) = CChar (i - j) ;    (CChar i) * (CChar j) = CChar (i * j) ;    negate  (CChar i) = CChar (negate i) ;    abs     (CChar i) = CChar (abs    i) ;    signum  (CChar i) = CChar (signum i) ;    fromInteger x = CChar (fromInteger x) } ; instance Real CChar where {    toRational (CChar i) = toRational i } ; instance Read CChar where {    readsPrec p s = map (\(x, t) -> (CChar x, t)) (readsPrec p s) } ; instance Show CChar where {    showsPrec p (CChar x) = showsPrec p x } ; instance Enum CChar where {    succ           (CChar i)             = CChar (succ i) ;    pred           (CChar i)             = CChar (pred i) ;    toEnum               x           = CChar (toEnum x) ;    fromEnum       (CChar i)             = fromEnum i ;    enumFrom       (CChar i)             = map CChar (enumFrom i) ;    enumFromThen   (CChar i) (CChar j)       = map CChar (enumFromThen i j) ;    enumFromTo     (CChar i) (CChar j)       = map CChar (enumFromTo i j) ;    enumFromThenTo (CChar i) (CChar j) (CChar k) = map CChar (enumFromThenTo i j k) } ; instance Storable CChar where {    sizeOf    (CChar x)       = sizeOf x ;    alignment (CChar x)       = alignment x ;    peekElemOff a i       = liftM CChar (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CChar x) = pokeElemOff (castPtr a) i x } ; tyConCChar = mkTyCon "CChar"; instance Typeable CChar where { typeOf _ = mkTyConApp tyConCChar [] } ; ; instance Bounded CChar where {    minBound = CChar minBound ;    maxBound = CChar maxBound } ; instance Integral CChar where {    (CChar i) `quot`    (CChar j) = CChar (i `quot` j) ;    (CChar i) `rem`     (CChar j) = CChar (i `rem`  j) ;    (CChar i) `div`     (CChar j) = CChar (i `div`  j) ;    (CChar i) `mod`     (CChar j) = CChar (i `mod`  j) ;    (CChar i) `quotRem` (CChar j) = let (q,r) = i `quotRem` j in (CChar q, CChar r) ;    (CChar i) `divMod`  (CChar j) = let (d,m) = i `divMod`  j in (CChar d, CChar m) ;    toInteger (CChar i)       = toInteger i } ; instance Bits CChar where {   (CChar x) .&.     (CChar y)   = CChar (x .&.   y) ;   (CChar x) .|.     (CChar y)   = CChar (x .|.   y) ;   (CChar x) `xor`   (CChar y)   = CChar (x `xor` y) ;   complement    (CChar x)   = CChar (complement x) ;   shift         (CChar x) n = CChar (shift x n) ;   rotate        (CChar x) n = CChar (rotate x n) ;   bit                 n = CChar (bit n) ;   setBit        (CChar x) n = CChar (setBit x n) ;   clearBit      (CChar x) n = CChar (clearBit x n) ;   complementBit (CChar x) n = CChar (complementBit x n) ;   testBit       (CChar x) n = testBit x n ;   bitSize       (CChar x)   = bitSize x ;   isSigned      (CChar x)   = isSigned x }
-- | Haskell type representing the C @signed char@ type.
newtype CSChar = CSChar Int8 deriving (Eq, Ord) ; instance Num CSChar where {    (CSChar i) + (CSChar j) = CSChar (i + j) ;    (CSChar i) - (CSChar j) = CSChar (i - j) ;    (CSChar i) * (CSChar j) = CSChar (i * j) ;    negate  (CSChar i) = CSChar (negate i) ;    abs     (CSChar i) = CSChar (abs    i) ;    signum  (CSChar i) = CSChar (signum i) ;    fromInteger x = CSChar (fromInteger x) } ; instance Real CSChar where {    toRational (CSChar i) = toRational i } ; instance Read CSChar where {    readsPrec p s = map (\(x, t) -> (CSChar x, t)) (readsPrec p s) } ; instance Show CSChar where {    showsPrec p (CSChar x) = showsPrec p x } ; instance Enum CSChar where {    succ           (CSChar i)             = CSChar (succ i) ;    pred           (CSChar i)             = CSChar (pred i) ;    toEnum               x           = CSChar (toEnum x) ;    fromEnum       (CSChar i)             = fromEnum i ;    enumFrom       (CSChar i)             = map CSChar (enumFrom i) ;    enumFromThen   (CSChar i) (CSChar j)       = map CSChar (enumFromThen i j) ;    enumFromTo     (CSChar i) (CSChar j)       = map CSChar (enumFromTo i j) ;    enumFromThenTo (CSChar i) (CSChar j) (CSChar k) = map CSChar (enumFromThenTo i j k) } ; instance Storable CSChar where {    sizeOf    (CSChar x)       = sizeOf x ;    alignment (CSChar x)       = alignment x ;    peekElemOff a i       = liftM CSChar (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CSChar x) = pokeElemOff (castPtr a) i x } ; tyConCSChar = mkTyCon "CSChar"; instance Typeable CSChar where { typeOf _ = mkTyConApp tyConCSChar [] } ; ; instance Bounded CSChar where {    minBound = CSChar minBound ;    maxBound = CSChar maxBound } ; instance Integral CSChar where {    (CSChar i) `quot`    (CSChar j) = CSChar (i `quot` j) ;    (CSChar i) `rem`     (CSChar j) = CSChar (i `rem`  j) ;    (CSChar i) `div`     (CSChar j) = CSChar (i `div`  j) ;    (CSChar i) `mod`     (CSChar j) = CSChar (i `mod`  j) ;    (CSChar i) `quotRem` (CSChar j) = let (q,r) = i `quotRem` j in (CSChar q, CSChar r) ;    (CSChar i) `divMod`  (CSChar j) = let (d,m) = i `divMod`  j in (CSChar d, CSChar m) ;    toInteger (CSChar i)       = toInteger i } ; instance Bits CSChar where {   (CSChar x) .&.     (CSChar y)   = CSChar (x .&.   y) ;   (CSChar x) .|.     (CSChar y)   = CSChar (x .|.   y) ;   (CSChar x) `xor`   (CSChar y)   = CSChar (x `xor` y) ;   complement    (CSChar x)   = CSChar (complement x) ;   shift         (CSChar x) n = CSChar (shift x n) ;   rotate        (CSChar x) n = CSChar (rotate x n) ;   bit                 n = CSChar (bit n) ;   setBit        (CSChar x) n = CSChar (setBit x n) ;   clearBit      (CSChar x) n = CSChar (clearBit x n) ;   complementBit (CSChar x) n = CSChar (complementBit x n) ;   testBit       (CSChar x) n = testBit x n ;   bitSize       (CSChar x)   = bitSize x ;   isSigned      (CSChar x)   = isSigned x }
-- | Haskell type representing the C @unsigned char@ type.
newtype CUChar = CUChar Word8 deriving (Eq, Ord) ; instance Num CUChar where {    (CUChar i) + (CUChar j) = CUChar (i + j) ;    (CUChar i) - (CUChar j) = CUChar (i - j) ;    (CUChar i) * (CUChar j) = CUChar (i * j) ;    negate  (CUChar i) = CUChar (negate i) ;    abs     (CUChar i) = CUChar (abs    i) ;    signum  (CUChar i) = CUChar (signum i) ;    fromInteger x = CUChar (fromInteger x) } ; instance Real CUChar where {    toRational (CUChar i) = toRational i } ; instance Read CUChar where {    readsPrec p s = map (\(x, t) -> (CUChar x, t)) (readsPrec p s) } ; instance Show CUChar where {    showsPrec p (CUChar x) = showsPrec p x } ; instance Enum CUChar where {    succ           (CUChar i)             = CUChar (succ i) ;    pred           (CUChar i)             = CUChar (pred i) ;    toEnum               x           = CUChar (toEnum x) ;    fromEnum       (CUChar i)             = fromEnum i ;    enumFrom       (CUChar i)             = map CUChar (enumFrom i) ;    enumFromThen   (CUChar i) (CUChar j)       = map CUChar (enumFromThen i j) ;    enumFromTo     (CUChar i) (CUChar j)       = map CUChar (enumFromTo i j) ;    enumFromThenTo (CUChar i) (CUChar j) (CUChar k) = map CUChar (enumFromThenTo i j k) } ; instance Storable CUChar where {    sizeOf    (CUChar x)       = sizeOf x ;    alignment (CUChar x)       = alignment x ;    peekElemOff a i       = liftM CUChar (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CUChar x) = pokeElemOff (castPtr a) i x } ; tyConCUChar = mkTyCon "CUChar"; instance Typeable CUChar where { typeOf _ = mkTyConApp tyConCUChar [] } ; ; instance Bounded CUChar where {    minBound = CUChar minBound ;    maxBound = CUChar maxBound } ; instance Integral CUChar where {    (CUChar i) `quot`    (CUChar j) = CUChar (i `quot` j) ;    (CUChar i) `rem`     (CUChar j) = CUChar (i `rem`  j) ;    (CUChar i) `div`     (CUChar j) = CUChar (i `div`  j) ;    (CUChar i) `mod`     (CUChar j) = CUChar (i `mod`  j) ;    (CUChar i) `quotRem` (CUChar j) = let (q,r) = i `quotRem` j in (CUChar q, CUChar r) ;    (CUChar i) `divMod`  (CUChar j) = let (d,m) = i `divMod`  j in (CUChar d, CUChar m) ;    toInteger (CUChar i)       = toInteger i } ; instance Bits CUChar where {   (CUChar x) .&.     (CUChar y)   = CUChar (x .&.   y) ;   (CUChar x) .|.     (CUChar y)   = CUChar (x .|.   y) ;   (CUChar x) `xor`   (CUChar y)   = CUChar (x `xor` y) ;   complement    (CUChar x)   = CUChar (complement x) ;   shift         (CUChar x) n = CUChar (shift x n) ;   rotate        (CUChar x) n = CUChar (rotate x n) ;   bit                 n = CUChar (bit n) ;   setBit        (CUChar x) n = CUChar (setBit x n) ;   clearBit      (CUChar x) n = CUChar (clearBit x n) ;   complementBit (CUChar x) n = CUChar (complementBit x n) ;   testBit       (CUChar x) n = testBit x n ;   bitSize       (CUChar x)   = bitSize x ;   isSigned      (CUChar x)   = isSigned x }

-- | Haskell type representing the C @short@ type.
newtype CShort = CShort Int16 deriving (Eq, Ord) ; instance Num CShort where {    (CShort i) + (CShort j) = CShort (i + j) ;    (CShort i) - (CShort j) = CShort (i - j) ;    (CShort i) * (CShort j) = CShort (i * j) ;    negate  (CShort i) = CShort (negate i) ;    abs     (CShort i) = CShort (abs    i) ;    signum  (CShort i) = CShort (signum i) ;    fromInteger x = CShort (fromInteger x) } ; instance Real CShort where {    toRational (CShort i) = toRational i } ; instance Read CShort where {    readsPrec p s = map (\(x, t) -> (CShort x, t)) (readsPrec p s) } ; instance Show CShort where {    showsPrec p (CShort x) = showsPrec p x } ; instance Enum CShort where {    succ           (CShort i)             = CShort (succ i) ;    pred           (CShort i)             = CShort (pred i) ;    toEnum               x           = CShort (toEnum x) ;    fromEnum       (CShort i)             = fromEnum i ;    enumFrom       (CShort i)             = map CShort (enumFrom i) ;    enumFromThen   (CShort i) (CShort j)       = map CShort (enumFromThen i j) ;    enumFromTo     (CShort i) (CShort j)       = map CShort (enumFromTo i j) ;    enumFromThenTo (CShort i) (CShort j) (CShort k) = map CShort (enumFromThenTo i j k) } ; instance Storable CShort where {    sizeOf    (CShort x)       = sizeOf x ;    alignment (CShort x)       = alignment x ;    peekElemOff a i       = liftM CShort (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CShort x) = pokeElemOff (castPtr a) i x } ; tyConCShort = mkTyCon "CShort"; instance Typeable CShort where { typeOf _ = mkTyConApp tyConCShort [] } ; ; instance Bounded CShort where {    minBound = CShort minBound ;    maxBound = CShort maxBound } ; instance Integral CShort where {    (CShort i) `quot`    (CShort j) = CShort (i `quot` j) ;    (CShort i) `rem`     (CShort j) = CShort (i `rem`  j) ;    (CShort i) `div`     (CShort j) = CShort (i `div`  j) ;    (CShort i) `mod`     (CShort j) = CShort (i `mod`  j) ;    (CShort i) `quotRem` (CShort j) = let (q,r) = i `quotRem` j in (CShort q, CShort r) ;    (CShort i) `divMod`  (CShort j) = let (d,m) = i `divMod`  j in (CShort d, CShort m) ;    toInteger (CShort i)       = toInteger i } ; instance Bits CShort where {   (CShort x) .&.     (CShort y)   = CShort (x .&.   y) ;   (CShort x) .|.     (CShort y)   = CShort (x .|.   y) ;   (CShort x) `xor`   (CShort y)   = CShort (x `xor` y) ;   complement    (CShort x)   = CShort (complement x) ;   shift         (CShort x) n = CShort (shift x n) ;   rotate        (CShort x) n = CShort (rotate x n) ;   bit                 n = CShort (bit n) ;   setBit        (CShort x) n = CShort (setBit x n) ;   clearBit      (CShort x) n = CShort (clearBit x n) ;   complementBit (CShort x) n = CShort (complementBit x n) ;   testBit       (CShort x) n = testBit x n ;   bitSize       (CShort x)   = bitSize x ;   isSigned      (CShort x)   = isSigned x }
-- | Haskell type representing the C @unsigned short@ type.
newtype CUShort = CUShort Word16 deriving (Eq, Ord) ; instance Num CUShort where {    (CUShort i) + (CUShort j) = CUShort (i + j) ;    (CUShort i) - (CUShort j) = CUShort (i - j) ;    (CUShort i) * (CUShort j) = CUShort (i * j) ;    negate  (CUShort i) = CUShort (negate i) ;    abs     (CUShort i) = CUShort (abs    i) ;    signum  (CUShort i) = CUShort (signum i) ;    fromInteger x = CUShort (fromInteger x) } ; instance Real CUShort where {    toRational (CUShort i) = toRational i } ; instance Read CUShort where {    readsPrec p s = map (\(x, t) -> (CUShort x, t)) (readsPrec p s) } ; instance Show CUShort where {    showsPrec p (CUShort x) = showsPrec p x } ; instance Enum CUShort where {    succ           (CUShort i)             = CUShort (succ i) ;    pred           (CUShort i)             = CUShort (pred i) ;    toEnum               x           = CUShort (toEnum x) ;    fromEnum       (CUShort i)             = fromEnum i ;    enumFrom       (CUShort i)             = map CUShort (enumFrom i) ;    enumFromThen   (CUShort i) (CUShort j)       = map CUShort (enumFromThen i j) ;    enumFromTo     (CUShort i) (CUShort j)       = map CUShort (enumFromTo i j) ;    enumFromThenTo (CUShort i) (CUShort j) (CUShort k) = map CUShort (enumFromThenTo i j k) } ; instance Storable CUShort where {    sizeOf    (CUShort x)       = sizeOf x ;    alignment (CUShort x)       = alignment x ;    peekElemOff a i       = liftM CUShort (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CUShort x) = pokeElemOff (castPtr a) i x } ; tyConCUShort = mkTyCon "CUShort"; instance Typeable CUShort where { typeOf _ = mkTyConApp tyConCUShort [] } ; ; instance Bounded CUShort where {    minBound = CUShort minBound ;    maxBound = CUShort maxBound } ; instance Integral CUShort where {    (CUShort i) `quot`    (CUShort j) = CUShort (i `quot` j) ;    (CUShort i) `rem`     (CUShort j) = CUShort (i `rem`  j) ;    (CUShort i) `div`     (CUShort j) = CUShort (i `div`  j) ;    (CUShort i) `mod`     (CUShort j) = CUShort (i `mod`  j) ;    (CUShort i) `quotRem` (CUShort j) = let (q,r) = i `quotRem` j in (CUShort q, CUShort r) ;    (CUShort i) `divMod`  (CUShort j) = let (d,m) = i `divMod`  j in (CUShort d, CUShort m) ;    toInteger (CUShort i)       = toInteger i } ; instance Bits CUShort where {   (CUShort x) .&.     (CUShort y)   = CUShort (x .&.   y) ;   (CUShort x) .|.     (CUShort y)   = CUShort (x .|.   y) ;   (CUShort x) `xor`   (CUShort y)   = CUShort (x `xor` y) ;   complement    (CUShort x)   = CUShort (complement x) ;   shift         (CUShort x) n = CUShort (shift x n) ;   rotate        (CUShort x) n = CUShort (rotate x n) ;   bit                 n = CUShort (bit n) ;   setBit        (CUShort x) n = CUShort (setBit x n) ;   clearBit      (CUShort x) n = CUShort (clearBit x n) ;   complementBit (CUShort x) n = CUShort (complementBit x n) ;   testBit       (CUShort x) n = testBit x n ;   bitSize       (CUShort x)   = bitSize x ;   isSigned      (CUShort x)   = isSigned x }

-- | Haskell type representing the C @int@ type.
newtype CInt = CInt Int32 deriving (Eq, Ord) ; instance Num CInt where {    (CInt i) + (CInt j) = CInt (i + j) ;    (CInt i) - (CInt j) = CInt (i - j) ;    (CInt i) * (CInt j) = CInt (i * j) ;    negate  (CInt i) = CInt (negate i) ;    abs     (CInt i) = CInt (abs    i) ;    signum  (CInt i) = CInt (signum i) ;    fromInteger x = CInt (fromInteger x) } ; instance Real CInt where {    toRational (CInt i) = toRational i } ; instance Read CInt where {    readsPrec p s = map (\(x, t) -> (CInt x, t)) (readsPrec p s) } ; instance Show CInt where {    showsPrec p (CInt x) = showsPrec p x } ; instance Enum CInt where {    succ           (CInt i)             = CInt (succ i) ;    pred           (CInt i)             = CInt (pred i) ;    toEnum               x           = CInt (toEnum x) ;    fromEnum       (CInt i)             = fromEnum i ;    enumFrom       (CInt i)             = map CInt (enumFrom i) ;    enumFromThen   (CInt i) (CInt j)       = map CInt (enumFromThen i j) ;    enumFromTo     (CInt i) (CInt j)       = map CInt (enumFromTo i j) ;    enumFromThenTo (CInt i) (CInt j) (CInt k) = map CInt (enumFromThenTo i j k) } ; instance Storable CInt where {    sizeOf    (CInt x)       = sizeOf x ;    alignment (CInt x)       = alignment x ;    peekElemOff a i       = liftM CInt (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CInt x) = pokeElemOff (castPtr a) i x } ; tyConCInt = mkTyCon "CInt"; instance Typeable CInt where { typeOf _ = mkTyConApp tyConCInt [] } ; ; instance Bounded CInt where {    minBound = CInt minBound ;    maxBound = CInt maxBound } ; instance Integral CInt where {    (CInt i) `quot`    (CInt j) = CInt (i `quot` j) ;    (CInt i) `rem`     (CInt j) = CInt (i `rem`  j) ;    (CInt i) `div`     (CInt j) = CInt (i `div`  j) ;    (CInt i) `mod`     (CInt j) = CInt (i `mod`  j) ;    (CInt i) `quotRem` (CInt j) = let (q,r) = i `quotRem` j in (CInt q, CInt r) ;    (CInt i) `divMod`  (CInt j) = let (d,m) = i `divMod`  j in (CInt d, CInt m) ;    toInteger (CInt i)       = toInteger i } ; instance Bits CInt where {   (CInt x) .&.     (CInt y)   = CInt (x .&.   y) ;   (CInt x) .|.     (CInt y)   = CInt (x .|.   y) ;   (CInt x) `xor`   (CInt y)   = CInt (x `xor` y) ;   complement    (CInt x)   = CInt (complement x) ;   shift         (CInt x) n = CInt (shift x n) ;   rotate        (CInt x) n = CInt (rotate x n) ;   bit                 n = CInt (bit n) ;   setBit        (CInt x) n = CInt (setBit x n) ;   clearBit      (CInt x) n = CInt (clearBit x n) ;   complementBit (CInt x) n = CInt (complementBit x n) ;   testBit       (CInt x) n = testBit x n ;   bitSize       (CInt x)   = bitSize x ;   isSigned      (CInt x)   = isSigned x }
-- | Haskell type representing the C @unsigned int@ type.
newtype CUInt = CUInt Word32 deriving (Eq, Ord) ; instance Num CUInt where {    (CUInt i) + (CUInt j) = CUInt (i + j) ;    (CUInt i) - (CUInt j) = CUInt (i - j) ;    (CUInt i) * (CUInt j) = CUInt (i * j) ;    negate  (CUInt i) = CUInt (negate i) ;    abs     (CUInt i) = CUInt (abs    i) ;    signum  (CUInt i) = CUInt (signum i) ;    fromInteger x = CUInt (fromInteger x) } ; instance Real CUInt where {    toRational (CUInt i) = toRational i } ; instance Read CUInt where {    readsPrec p s = map (\(x, t) -> (CUInt x, t)) (readsPrec p s) } ; instance Show CUInt where {    showsPrec p (CUInt x) = showsPrec p x } ; instance Enum CUInt where {    succ           (CUInt i)             = CUInt (succ i) ;    pred           (CUInt i)             = CUInt (pred i) ;    toEnum               x           = CUInt (toEnum x) ;    fromEnum       (CUInt i)             = fromEnum i ;    enumFrom       (CUInt i)             = map CUInt (enumFrom i) ;    enumFromThen   (CUInt i) (CUInt j)       = map CUInt (enumFromThen i j) ;    enumFromTo     (CUInt i) (CUInt j)       = map CUInt (enumFromTo i j) ;    enumFromThenTo (CUInt i) (CUInt j) (CUInt k) = map CUInt (enumFromThenTo i j k) } ; instance Storable CUInt where {    sizeOf    (CUInt x)       = sizeOf x ;    alignment (CUInt x)       = alignment x ;    peekElemOff a i       = liftM CUInt (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CUInt x) = pokeElemOff (castPtr a) i x } ; tyConCUInt = mkTyCon "CUInt"; instance Typeable CUInt where { typeOf _ = mkTyConApp tyConCUInt [] } ; ; instance Bounded CUInt where {    minBound = CUInt minBound ;    maxBound = CUInt maxBound } ; instance Integral CUInt where {    (CUInt i) `quot`    (CUInt j) = CUInt (i `quot` j) ;    (CUInt i) `rem`     (CUInt j) = CUInt (i `rem`  j) ;    (CUInt i) `div`     (CUInt j) = CUInt (i `div`  j) ;    (CUInt i) `mod`     (CUInt j) = CUInt (i `mod`  j) ;    (CUInt i) `quotRem` (CUInt j) = let (q,r) = i `quotRem` j in (CUInt q, CUInt r) ;    (CUInt i) `divMod`  (CUInt j) = let (d,m) = i `divMod`  j in (CUInt d, CUInt m) ;    toInteger (CUInt i)       = toInteger i } ; instance Bits CUInt where {   (CUInt x) .&.     (CUInt y)   = CUInt (x .&.   y) ;   (CUInt x) .|.     (CUInt y)   = CUInt (x .|.   y) ;   (CUInt x) `xor`   (CUInt y)   = CUInt (x `xor` y) ;   complement    (CUInt x)   = CUInt (complement x) ;   shift         (CUInt x) n = CUInt (shift x n) ;   rotate        (CUInt x) n = CUInt (rotate x n) ;   bit                 n = CUInt (bit n) ;   setBit        (CUInt x) n = CUInt (setBit x n) ;   clearBit      (CUInt x) n = CUInt (clearBit x n) ;   complementBit (CUInt x) n = CUInt (complementBit x n) ;   testBit       (CUInt x) n = testBit x n ;   bitSize       (CUInt x)   = bitSize x ;   isSigned      (CUInt x)   = isSigned x }

-- | Haskell type representing the C @long@ type.
newtype CLong = CLong Int64 deriving (Eq, Ord) ; instance Num CLong where {    (CLong i) + (CLong j) = CLong (i + j) ;    (CLong i) - (CLong j) = CLong (i - j) ;    (CLong i) * (CLong j) = CLong (i * j) ;    negate  (CLong i) = CLong (negate i) ;    abs     (CLong i) = CLong (abs    i) ;    signum  (CLong i) = CLong (signum i) ;    fromInteger x = CLong (fromInteger x) } ; instance Real CLong where {    toRational (CLong i) = toRational i } ; instance Read CLong where {    readsPrec p s = map (\(x, t) -> (CLong x, t)) (readsPrec p s) } ; instance Show CLong where {    showsPrec p (CLong x) = showsPrec p x } ; instance Enum CLong where {    succ           (CLong i)             = CLong (succ i) ;    pred           (CLong i)             = CLong (pred i) ;    toEnum               x           = CLong (toEnum x) ;    fromEnum       (CLong i)             = fromEnum i ;    enumFrom       (CLong i)             = map CLong (enumFrom i) ;    enumFromThen   (CLong i) (CLong j)       = map CLong (enumFromThen i j) ;    enumFromTo     (CLong i) (CLong j)       = map CLong (enumFromTo i j) ;    enumFromThenTo (CLong i) (CLong j) (CLong k) = map CLong (enumFromThenTo i j k) } ; instance Storable CLong where {    sizeOf    (CLong x)       = sizeOf x ;    alignment (CLong x)       = alignment x ;    peekElemOff a i       = liftM CLong (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CLong x) = pokeElemOff (castPtr a) i x } ; tyConCLong = mkTyCon "CLong"; instance Typeable CLong where { typeOf _ = mkTyConApp tyConCLong [] } ; ; instance Bounded CLong where {    minBound = CLong minBound ;    maxBound = CLong maxBound } ; instance Integral CLong where {    (CLong i) `quot`    (CLong j) = CLong (i `quot` j) ;    (CLong i) `rem`     (CLong j) = CLong (i `rem`  j) ;    (CLong i) `div`     (CLong j) = CLong (i `div`  j) ;    (CLong i) `mod`     (CLong j) = CLong (i `mod`  j) ;    (CLong i) `quotRem` (CLong j) = let (q,r) = i `quotRem` j in (CLong q, CLong r) ;    (CLong i) `divMod`  (CLong j) = let (d,m) = i `divMod`  j in (CLong d, CLong m) ;    toInteger (CLong i)       = toInteger i } ; instance Bits CLong where {   (CLong x) .&.     (CLong y)   = CLong (x .&.   y) ;   (CLong x) .|.     (CLong y)   = CLong (x .|.   y) ;   (CLong x) `xor`   (CLong y)   = CLong (x `xor` y) ;   complement    (CLong x)   = CLong (complement x) ;   shift         (CLong x) n = CLong (shift x n) ;   rotate        (CLong x) n = CLong (rotate x n) ;   bit                 n = CLong (bit n) ;   setBit        (CLong x) n = CLong (setBit x n) ;   clearBit      (CLong x) n = CLong (clearBit x n) ;   complementBit (CLong x) n = CLong (complementBit x n) ;   testBit       (CLong x) n = testBit x n ;   bitSize       (CLong x)   = bitSize x ;   isSigned      (CLong x)   = isSigned x }
-- | Haskell type representing the C @unsigned long@ type.
newtype CULong = CULong Word64 deriving (Eq, Ord) ; instance Num CULong where {    (CULong i) + (CULong j) = CULong (i + j) ;    (CULong i) - (CULong j) = CULong (i - j) ;    (CULong i) * (CULong j) = CULong (i * j) ;    negate  (CULong i) = CULong (negate i) ;    abs     (CULong i) = CULong (abs    i) ;    signum  (CULong i) = CULong (signum i) ;    fromInteger x = CULong (fromInteger x) } ; instance Real CULong where {    toRational (CULong i) = toRational i } ; instance Read CULong where {    readsPrec p s = map (\(x, t) -> (CULong x, t)) (readsPrec p s) } ; instance Show CULong where {    showsPrec p (CULong x) = showsPrec p x } ; instance Enum CULong where {    succ           (CULong i)             = CULong (succ i) ;    pred           (CULong i)             = CULong (pred i) ;    toEnum               x           = CULong (toEnum x) ;    fromEnum       (CULong i)             = fromEnum i ;    enumFrom       (CULong i)             = map CULong (enumFrom i) ;    enumFromThen   (CULong i) (CULong j)       = map CULong (enumFromThen i j) ;    enumFromTo     (CULong i) (CULong j)       = map CULong (enumFromTo i j) ;    enumFromThenTo (CULong i) (CULong j) (CULong k) = map CULong (enumFromThenTo i j k) } ; instance Storable CULong where {    sizeOf    (CULong x)       = sizeOf x ;    alignment (CULong x)       = alignment x ;    peekElemOff a i       = liftM CULong (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CULong x) = pokeElemOff (castPtr a) i x } ; tyConCULong = mkTyCon "CULong"; instance Typeable CULong where { typeOf _ = mkTyConApp tyConCULong [] } ; ; instance Bounded CULong where {    minBound = CULong minBound ;    maxBound = CULong maxBound } ; instance Integral CULong where {    (CULong i) `quot`    (CULong j) = CULong (i `quot` j) ;    (CULong i) `rem`     (CULong j) = CULong (i `rem`  j) ;    (CULong i) `div`     (CULong j) = CULong (i `div`  j) ;    (CULong i) `mod`     (CULong j) = CULong (i `mod`  j) ;    (CULong i) `quotRem` (CULong j) = let (q,r) = i `quotRem` j in (CULong q, CULong r) ;    (CULong i) `divMod`  (CULong j) = let (d,m) = i `divMod`  j in (CULong d, CULong m) ;    toInteger (CULong i)       = toInteger i } ; instance Bits CULong where {   (CULong x) .&.     (CULong y)   = CULong (x .&.   y) ;   (CULong x) .|.     (CULong y)   = CULong (x .|.   y) ;   (CULong x) `xor`   (CULong y)   = CULong (x `xor` y) ;   complement    (CULong x)   = CULong (complement x) ;   shift         (CULong x) n = CULong (shift x n) ;   rotate        (CULong x) n = CULong (rotate x n) ;   bit                 n = CULong (bit n) ;   setBit        (CULong x) n = CULong (setBit x n) ;   clearBit      (CULong x) n = CULong (clearBit x n) ;   complementBit (CULong x) n = CULong (complementBit x n) ;   testBit       (CULong x) n = testBit x n ;   bitSize       (CULong x)   = bitSize x ;   isSigned      (CULong x)   = isSigned x }

-- | Haskell type representing the C @long long@ type.
newtype CLLong = CLLong Int64 deriving (Eq, Ord) ; instance Num CLLong where {    (CLLong i) + (CLLong j) = CLLong (i + j) ;    (CLLong i) - (CLLong j) = CLLong (i - j) ;    (CLLong i) * (CLLong j) = CLLong (i * j) ;    negate  (CLLong i) = CLLong (negate i) ;    abs     (CLLong i) = CLLong (abs    i) ;    signum  (CLLong i) = CLLong (signum i) ;    fromInteger x = CLLong (fromInteger x) } ; instance Real CLLong where {    toRational (CLLong i) = toRational i } ; instance Read CLLong where {    readsPrec p s = map (\(x, t) -> (CLLong x, t)) (readsPrec p s) } ; instance Show CLLong where {    showsPrec p (CLLong x) = showsPrec p x } ; instance Enum CLLong where {    succ           (CLLong i)             = CLLong (succ i) ;    pred           (CLLong i)             = CLLong (pred i) ;    toEnum               x           = CLLong (toEnum x) ;    fromEnum       (CLLong i)             = fromEnum i ;    enumFrom       (CLLong i)             = map CLLong (enumFrom i) ;    enumFromThen   (CLLong i) (CLLong j)       = map CLLong (enumFromThen i j) ;    enumFromTo     (CLLong i) (CLLong j)       = map CLLong (enumFromTo i j) ;    enumFromThenTo (CLLong i) (CLLong j) (CLLong k) = map CLLong (enumFromThenTo i j k) } ; instance Storable CLLong where {    sizeOf    (CLLong x)       = sizeOf x ;    alignment (CLLong x)       = alignment x ;    peekElemOff a i       = liftM CLLong (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CLLong x) = pokeElemOff (castPtr a) i x } ; tyConCLLong = mkTyCon "CLLong"; instance Typeable CLLong where { typeOf _ = mkTyConApp tyConCLLong [] } ; ; instance Bounded CLLong where {    minBound = CLLong minBound ;    maxBound = CLLong maxBound } ; instance Integral CLLong where {    (CLLong i) `quot`    (CLLong j) = CLLong (i `quot` j) ;    (CLLong i) `rem`     (CLLong j) = CLLong (i `rem`  j) ;    (CLLong i) `div`     (CLLong j) = CLLong (i `div`  j) ;    (CLLong i) `mod`     (CLLong j) = CLLong (i `mod`  j) ;    (CLLong i) `quotRem` (CLLong j) = let (q,r) = i `quotRem` j in (CLLong q, CLLong r) ;    (CLLong i) `divMod`  (CLLong j) = let (d,m) = i `divMod`  j in (CLLong d, CLLong m) ;    toInteger (CLLong i)       = toInteger i } ; instance Bits CLLong where {   (CLLong x) .&.     (CLLong y)   = CLLong (x .&.   y) ;   (CLLong x) .|.     (CLLong y)   = CLLong (x .|.   y) ;   (CLLong x) `xor`   (CLLong y)   = CLLong (x `xor` y) ;   complement    (CLLong x)   = CLLong (complement x) ;   shift         (CLLong x) n = CLLong (shift x n) ;   rotate        (CLLong x) n = CLLong (rotate x n) ;   bit                 n = CLLong (bit n) ;   setBit        (CLLong x) n = CLLong (setBit x n) ;   clearBit      (CLLong x) n = CLLong (clearBit x n) ;   complementBit (CLLong x) n = CLLong (complementBit x n) ;   testBit       (CLLong x) n = testBit x n ;   bitSize       (CLLong x)   = bitSize x ;   isSigned      (CLLong x)   = isSigned x }
-- | Haskell type representing the C @unsigned long long@ type.
newtype CULLong = CULLong Word64 deriving (Eq, Ord) ; instance Num CULLong where {    (CULLong i) + (CULLong j) = CULLong (i + j) ;    (CULLong i) - (CULLong j) = CULLong (i - j) ;    (CULLong i) * (CULLong j) = CULLong (i * j) ;    negate  (CULLong i) = CULLong (negate i) ;    abs     (CULLong i) = CULLong (abs    i) ;    signum  (CULLong i) = CULLong (signum i) ;    fromInteger x = CULLong (fromInteger x) } ; instance Real CULLong where {    toRational (CULLong i) = toRational i } ; instance Read CULLong where {    readsPrec p s = map (\(x, t) -> (CULLong x, t)) (readsPrec p s) } ; instance Show CULLong where {    showsPrec p (CULLong x) = showsPrec p x } ; instance Enum CULLong where {    succ           (CULLong i)             = CULLong (succ i) ;    pred           (CULLong i)             = CULLong (pred i) ;    toEnum               x           = CULLong (toEnum x) ;    fromEnum       (CULLong i)             = fromEnum i ;    enumFrom       (CULLong i)             = map CULLong (enumFrom i) ;    enumFromThen   (CULLong i) (CULLong j)       = map CULLong (enumFromThen i j) ;    enumFromTo     (CULLong i) (CULLong j)       = map CULLong (enumFromTo i j) ;    enumFromThenTo (CULLong i) (CULLong j) (CULLong k) = map CULLong (enumFromThenTo i j k) } ; instance Storable CULLong where {    sizeOf    (CULLong x)       = sizeOf x ;    alignment (CULLong x)       = alignment x ;    peekElemOff a i       = liftM CULLong (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CULLong x) = pokeElemOff (castPtr a) i x } ; tyConCULLong = mkTyCon "CULLong"; instance Typeable CULLong where { typeOf _ = mkTyConApp tyConCULLong [] } ; ; instance Bounded CULLong where {    minBound = CULLong minBound ;    maxBound = CULLong maxBound } ; instance Integral CULLong where {    (CULLong i) `quot`    (CULLong j) = CULLong (i `quot` j) ;    (CULLong i) `rem`     (CULLong j) = CULLong (i `rem`  j) ;    (CULLong i) `div`     (CULLong j) = CULLong (i `div`  j) ;    (CULLong i) `mod`     (CULLong j) = CULLong (i `mod`  j) ;    (CULLong i) `quotRem` (CULLong j) = let (q,r) = i `quotRem` j in (CULLong q, CULLong r) ;    (CULLong i) `divMod`  (CULLong j) = let (d,m) = i `divMod`  j in (CULLong d, CULLong m) ;    toInteger (CULLong i)       = toInteger i } ; instance Bits CULLong where {   (CULLong x) .&.     (CULLong y)   = CULLong (x .&.   y) ;   (CULLong x) .|.     (CULLong y)   = CULLong (x .|.   y) ;   (CULLong x) `xor`   (CULLong y)   = CULLong (x `xor` y) ;   complement    (CULLong x)   = CULLong (complement x) ;   shift         (CULLong x) n = CULLong (shift x n) ;   rotate        (CULLong x) n = CULLong (rotate x n) ;   bit                 n = CULLong (bit n) ;   setBit        (CULLong x) n = CULLong (setBit x n) ;   clearBit      (CULLong x) n = CULLong (clearBit x n) ;   complementBit (CULLong x) n = CULLong (complementBit x n) ;   testBit       (CULLong x) n = testBit x n ;   bitSize       (CULLong x)   = bitSize x ;   isSigned      (CULLong x)   = isSigned x }

{-# RULES
"fromIntegral/a->CChar"   fromIntegral = \x -> CChar   (fromIntegral x)
"fromIntegral/a->CSChar"  fromIntegral = \x -> CSChar  (fromIntegral x)
"fromIntegral/a->CUChar"  fromIntegral = \x -> CUChar  (fromIntegral x)
"fromIntegral/a->CShort"  fromIntegral = \x -> CShort  (fromIntegral x)
"fromIntegral/a->CUShort" fromIntegral = \x -> CUShort (fromIntegral x)
"fromIntegral/a->CInt"    fromIntegral = \x -> CInt    (fromIntegral x)
"fromIntegral/a->CUInt"   fromIntegral = \x -> CUInt   (fromIntegral x)
"fromIntegral/a->CLong"   fromIntegral = \x -> CLong   (fromIntegral x)
"fromIntegral/a->CULong"  fromIntegral = \x -> CULong  (fromIntegral x)
"fromIntegral/a->CLLong"  fromIntegral = \x -> CLLong  (fromIntegral x)
"fromIntegral/a->CULLong" fromIntegral = \x -> CULLong (fromIntegral x)

"fromIntegral/CChar->a"   fromIntegral = \(CChar   x) -> fromIntegral x
"fromIntegral/CSChar->a"  fromIntegral = \(CSChar  x) -> fromIntegral x
"fromIntegral/CUChar->a"  fromIntegral = \(CUChar  x) -> fromIntegral x
"fromIntegral/CShort->a"  fromIntegral = \(CShort  x) -> fromIntegral x
"fromIntegral/CUShort->a" fromIntegral = \(CUShort x) -> fromIntegral x
"fromIntegral/CInt->a"    fromIntegral = \(CInt    x) -> fromIntegral x
"fromIntegral/CUInt->a"   fromIntegral = \(CUInt   x) -> fromIntegral x
"fromIntegral/CLong->a"   fromIntegral = \(CLong   x) -> fromIntegral x
"fromIntegral/CULong->a"  fromIntegral = \(CULong  x) -> fromIntegral x
"fromIntegral/CLLong->a"  fromIntegral = \(CLLong  x) -> fromIntegral x
"fromIntegral/CULLong->a" fromIntegral = \(CULLong x) -> fromIntegral x
 #-}

-- | Haskell type representing the C @float@ type.
newtype CFloat = CFloat Float deriving (Eq, Ord) ; instance Num CFloat where {    (CFloat i) + (CFloat j) = CFloat (i + j) ;    (CFloat i) - (CFloat j) = CFloat (i - j) ;    (CFloat i) * (CFloat j) = CFloat (i * j) ;    negate  (CFloat i) = CFloat (negate i) ;    abs     (CFloat i) = CFloat (abs    i) ;    signum  (CFloat i) = CFloat (signum i) ;    fromInteger x = CFloat (fromInteger x) } ; instance Real CFloat where {    toRational (CFloat i) = toRational i } ; instance Read CFloat where {    readsPrec p s = map (\(x, t) -> (CFloat x, t)) (readsPrec p s) } ; instance Show CFloat where {    showsPrec p (CFloat x) = showsPrec p x } ; instance Enum CFloat where {    succ           (CFloat i)             = CFloat (succ i) ;    pred           (CFloat i)             = CFloat (pred i) ;    toEnum               x           = CFloat (toEnum x) ;    fromEnum       (CFloat i)             = fromEnum i ;    enumFrom       (CFloat i)             = map CFloat (enumFrom i) ;    enumFromThen   (CFloat i) (CFloat j)       = map CFloat (enumFromThen i j) ;    enumFromTo     (CFloat i) (CFloat j)       = map CFloat (enumFromTo i j) ;    enumFromThenTo (CFloat i) (CFloat j) (CFloat k) = map CFloat (enumFromThenTo i j k) } ; instance Storable CFloat where {    sizeOf    (CFloat x)       = sizeOf x ;    alignment (CFloat x)       = alignment x ;    peekElemOff a i       = liftM CFloat (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CFloat x) = pokeElemOff (castPtr a) i x } ; tyConCFloat = mkTyCon "CFloat"; instance Typeable CFloat where { typeOf _ = mkTyConApp tyConCFloat [] } ; ; instance Fractional CFloat where {    (CFloat x) / (CFloat y)  = CFloat (x / y) ;    recip   (CFloat x)  = CFloat (recip x) ;    fromRational	r = CFloat (fromRational r) } ; instance Floating CFloat where {    pi                    = pi ;    exp   (CFloat x)           = CFloat (exp   x) ;    log   (CFloat x)           = CFloat (log   x) ;    sqrt  (CFloat x)           = CFloat (sqrt  x) ;    (CFloat x) **        (CFloat y) = CFloat (x ** y) ;    (CFloat x) `logBase` (CFloat y) = CFloat (x `logBase` y) ;    sin   (CFloat x)           = CFloat (sin   x) ;    cos   (CFloat x)           = CFloat (cos   x) ;    tan   (CFloat x)           = CFloat (tan   x) ;    asin  (CFloat x)           = CFloat (asin  x) ;    acos  (CFloat x)           = CFloat (acos  x) ;    atan  (CFloat x)           = CFloat (atan  x) ;    sinh  (CFloat x)           = CFloat (sinh  x) ;    cosh  (CFloat x)           = CFloat (cosh  x) ;    tanh  (CFloat x)           = CFloat (tanh  x) ;    asinh (CFloat x)           = CFloat (asinh x) ;    acosh (CFloat x)           = CFloat (acosh x) ;    atanh (CFloat x)           = CFloat (atanh x) } ; instance RealFrac CFloat where {    properFraction (CFloat x) = let (m,y) = properFraction x in (m, CFloat y) ;    truncate (CFloat x) = truncate x ;    round    (CFloat x) = round x ;    ceiling  (CFloat x) = ceiling x ;    floor    (CFloat x) = floor x } ; instance RealFloat CFloat where {    floatRadix     (CFloat x) = floatRadix x ;    floatDigits    (CFloat x) = floatDigits x ;    floatRange     (CFloat x) = floatRange x ;    decodeFloat    (CFloat x) = decodeFloat x ;    encodeFloat m n      = CFloat (encodeFloat m n) ;    exponent       (CFloat x) = exponent x ;    significand    (CFloat x) = CFloat (significand  x) ;    scaleFloat n   (CFloat x) = CFloat (scaleFloat n x) ;    isNaN          (CFloat x) = isNaN x ;    isInfinite     (CFloat x) = isInfinite x ;    isDenormalized (CFloat x) = isDenormalized x ;    isNegativeZero (CFloat x) = isNegativeZero x ;    isIEEE         (CFloat x) = isIEEE x ;    (CFloat x) `atan2`  (CFloat y) = CFloat (x `atan2` y) }
-- | Haskell type representing the C @double@ type.
newtype CDouble = CDouble Double deriving (Eq, Ord) ; instance Num CDouble where {    (CDouble i) + (CDouble j) = CDouble (i + j) ;    (CDouble i) - (CDouble j) = CDouble (i - j) ;    (CDouble i) * (CDouble j) = CDouble (i * j) ;    negate  (CDouble i) = CDouble (negate i) ;    abs     (CDouble i) = CDouble (abs    i) ;    signum  (CDouble i) = CDouble (signum i) ;    fromInteger x = CDouble (fromInteger x) } ; instance Real CDouble where {    toRational (CDouble i) = toRational i } ; instance Read CDouble where {    readsPrec p s = map (\(x, t) -> (CDouble x, t)) (readsPrec p s) } ; instance Show CDouble where {    showsPrec p (CDouble x) = showsPrec p x } ; instance Enum CDouble where {    succ           (CDouble i)             = CDouble (succ i) ;    pred           (CDouble i)             = CDouble (pred i) ;    toEnum               x           = CDouble (toEnum x) ;    fromEnum       (CDouble i)             = fromEnum i ;    enumFrom       (CDouble i)             = map CDouble (enumFrom i) ;    enumFromThen   (CDouble i) (CDouble j)       = map CDouble (enumFromThen i j) ;    enumFromTo     (CDouble i) (CDouble j)       = map CDouble (enumFromTo i j) ;    enumFromThenTo (CDouble i) (CDouble j) (CDouble k) = map CDouble (enumFromThenTo i j k) } ; instance Storable CDouble where {    sizeOf    (CDouble x)       = sizeOf x ;    alignment (CDouble x)       = alignment x ;    peekElemOff a i       = liftM CDouble (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CDouble x) = pokeElemOff (castPtr a) i x } ; tyConCDouble = mkTyCon "CDouble"; instance Typeable CDouble where { typeOf _ = mkTyConApp tyConCDouble [] } ; ; instance Fractional CDouble where {    (CDouble x) / (CDouble y)  = CDouble (x / y) ;    recip   (CDouble x)  = CDouble (recip x) ;    fromRational	r = CDouble (fromRational r) } ; instance Floating CDouble where {    pi                    = pi ;    exp   (CDouble x)           = CDouble (exp   x) ;    log   (CDouble x)           = CDouble (log   x) ;    sqrt  (CDouble x)           = CDouble (sqrt  x) ;    (CDouble x) **        (CDouble y) = CDouble (x ** y) ;    (CDouble x) `logBase` (CDouble y) = CDouble (x `logBase` y) ;    sin   (CDouble x)           = CDouble (sin   x) ;    cos   (CDouble x)           = CDouble (cos   x) ;    tan   (CDouble x)           = CDouble (tan   x) ;    asin  (CDouble x)           = CDouble (asin  x) ;    acos  (CDouble x)           = CDouble (acos  x) ;    atan  (CDouble x)           = CDouble (atan  x) ;    sinh  (CDouble x)           = CDouble (sinh  x) ;    cosh  (CDouble x)           = CDouble (cosh  x) ;    tanh  (CDouble x)           = CDouble (tanh  x) ;    asinh (CDouble x)           = CDouble (asinh x) ;    acosh (CDouble x)           = CDouble (acosh x) ;    atanh (CDouble x)           = CDouble (atanh x) } ; instance RealFrac CDouble where {    properFraction (CDouble x) = let (m,y) = properFraction x in (m, CDouble y) ;    truncate (CDouble x) = truncate x ;    round    (CDouble x) = round x ;    ceiling  (CDouble x) = ceiling x ;    floor    (CDouble x) = floor x } ; instance RealFloat CDouble where {    floatRadix     (CDouble x) = floatRadix x ;    floatDigits    (CDouble x) = floatDigits x ;    floatRange     (CDouble x) = floatRange x ;    decodeFloat    (CDouble x) = decodeFloat x ;    encodeFloat m n      = CDouble (encodeFloat m n) ;    exponent       (CDouble x) = exponent x ;    significand    (CDouble x) = CDouble (significand  x) ;    scaleFloat n   (CDouble x) = CDouble (scaleFloat n x) ;    isNaN          (CDouble x) = isNaN x ;    isInfinite     (CDouble x) = isInfinite x ;    isDenormalized (CDouble x) = isDenormalized x ;    isNegativeZero (CDouble x) = isNegativeZero x ;    isIEEE         (CDouble x) = isIEEE x ;    (CDouble x) `atan2`  (CDouble y) = CDouble (x `atan2` y) }
-- HACK: Currently no long double in the FFI, so we simply re-use double
-- | Haskell type representing the C @long double@ type.
newtype CLDouble = CLDouble Double deriving (Eq, Ord) ; instance Num CLDouble where {    (CLDouble i) + (CLDouble j) = CLDouble (i + j) ;    (CLDouble i) - (CLDouble j) = CLDouble (i - j) ;    (CLDouble i) * (CLDouble j) = CLDouble (i * j) ;    negate  (CLDouble i) = CLDouble (negate i) ;    abs     (CLDouble i) = CLDouble (abs    i) ;    signum  (CLDouble i) = CLDouble (signum i) ;    fromInteger x = CLDouble (fromInteger x) } ; instance Real CLDouble where {    toRational (CLDouble i) = toRational i } ; instance Read CLDouble where {    readsPrec p s = map (\(x, t) -> (CLDouble x, t)) (readsPrec p s) } ; instance Show CLDouble where {    showsPrec p (CLDouble x) = showsPrec p x } ; instance Enum CLDouble where {    succ           (CLDouble i)             = CLDouble (succ i) ;    pred           (CLDouble i)             = CLDouble (pred i) ;    toEnum               x           = CLDouble (toEnum x) ;    fromEnum       (CLDouble i)             = fromEnum i ;    enumFrom       (CLDouble i)             = map CLDouble (enumFrom i) ;    enumFromThen   (CLDouble i) (CLDouble j)       = map CLDouble (enumFromThen i j) ;    enumFromTo     (CLDouble i) (CLDouble j)       = map CLDouble (enumFromTo i j) ;    enumFromThenTo (CLDouble i) (CLDouble j) (CLDouble k) = map CLDouble (enumFromThenTo i j k) } ; instance Storable CLDouble where {    sizeOf    (CLDouble x)       = sizeOf x ;    alignment (CLDouble x)       = alignment x ;    peekElemOff a i       = liftM CLDouble (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CLDouble x) = pokeElemOff (castPtr a) i x } ; tyConCLDouble = mkTyCon "CLDouble"; instance Typeable CLDouble where { typeOf _ = mkTyConApp tyConCLDouble [] } ; ; instance Fractional CLDouble where {    (CLDouble x) / (CLDouble y)  = CLDouble (x / y) ;    recip   (CLDouble x)  = CLDouble (recip x) ;    fromRational	r = CLDouble (fromRational r) } ; instance Floating CLDouble where {    pi                    = pi ;    exp   (CLDouble x)           = CLDouble (exp   x) ;    log   (CLDouble x)           = CLDouble (log   x) ;    sqrt  (CLDouble x)           = CLDouble (sqrt  x) ;    (CLDouble x) **        (CLDouble y) = CLDouble (x ** y) ;    (CLDouble x) `logBase` (CLDouble y) = CLDouble (x `logBase` y) ;    sin   (CLDouble x)           = CLDouble (sin   x) ;    cos   (CLDouble x)           = CLDouble (cos   x) ;    tan   (CLDouble x)           = CLDouble (tan   x) ;    asin  (CLDouble x)           = CLDouble (asin  x) ;    acos  (CLDouble x)           = CLDouble (acos  x) ;    atan  (CLDouble x)           = CLDouble (atan  x) ;    sinh  (CLDouble x)           = CLDouble (sinh  x) ;    cosh  (CLDouble x)           = CLDouble (cosh  x) ;    tanh  (CLDouble x)           = CLDouble (tanh  x) ;    asinh (CLDouble x)           = CLDouble (asinh x) ;    acosh (CLDouble x)           = CLDouble (acosh x) ;    atanh (CLDouble x)           = CLDouble (atanh x) } ; instance RealFrac CLDouble where {    properFraction (CLDouble x) = let (m,y) = properFraction x in (m, CLDouble y) ;    truncate (CLDouble x) = truncate x ;    round    (CLDouble x) = round x ;    ceiling  (CLDouble x) = ceiling x ;    floor    (CLDouble x) = floor x } ; instance RealFloat CLDouble where {    floatRadix     (CLDouble x) = floatRadix x ;    floatDigits    (CLDouble x) = floatDigits x ;    floatRange     (CLDouble x) = floatRange x ;    decodeFloat    (CLDouble x) = decodeFloat x ;    encodeFloat m n      = CLDouble (encodeFloat m n) ;    exponent       (CLDouble x) = exponent x ;    significand    (CLDouble x) = CLDouble (significand  x) ;    scaleFloat n   (CLDouble x) = CLDouble (scaleFloat n x) ;    isNaN          (CLDouble x) = isNaN x ;    isInfinite     (CLDouble x) = isInfinite x ;    isDenormalized (CLDouble x) = isDenormalized x ;    isNegativeZero (CLDouble x) = isNegativeZero x ;    isIEEE         (CLDouble x) = isIEEE x ;    (CLDouble x) `atan2`  (CLDouble y) = CLDouble (x `atan2` y) }

{-# RULES
"realToFrac/a->CFloat"    realToFrac = \x -> CFloat   (realToFrac x)
"realToFrac/a->CDouble"   realToFrac = \x -> CDouble  (realToFrac x)
"realToFrac/a->CLDouble"  realToFrac = \x -> CLDouble (realToFrac x)

"realToFrac/CFloat->a"    realToFrac = \(CFloat   x) -> realToFrac x
"realToFrac/CDouble->a"   realToFrac = \(CDouble  x) -> realToFrac x
"realToFrac/CLDouble->a"  realToFrac = \(CLDouble x) -> realToFrac x
 #-}

-- | Haskell type representing the C @ptrdiff_t@ type.
newtype CPtrdiff = CPtrdiff Int64 deriving (Eq, Ord) ; instance Num CPtrdiff where {    (CPtrdiff i) + (CPtrdiff j) = CPtrdiff (i + j) ;    (CPtrdiff i) - (CPtrdiff j) = CPtrdiff (i - j) ;    (CPtrdiff i) * (CPtrdiff j) = CPtrdiff (i * j) ;    negate  (CPtrdiff i) = CPtrdiff (negate i) ;    abs     (CPtrdiff i) = CPtrdiff (abs    i) ;    signum  (CPtrdiff i) = CPtrdiff (signum i) ;    fromInteger x = CPtrdiff (fromInteger x) } ; instance Real CPtrdiff where {    toRational (CPtrdiff i) = toRational i } ; instance Read CPtrdiff where {    readsPrec p s = map (\(x, t) -> (CPtrdiff x, t)) (readsPrec p s) } ; instance Show CPtrdiff where {    showsPrec p (CPtrdiff x) = showsPrec p x } ; instance Enum CPtrdiff where {    succ           (CPtrdiff i)             = CPtrdiff (succ i) ;    pred           (CPtrdiff i)             = CPtrdiff (pred i) ;    toEnum               x           = CPtrdiff (toEnum x) ;    fromEnum       (CPtrdiff i)             = fromEnum i ;    enumFrom       (CPtrdiff i)             = map CPtrdiff (enumFrom i) ;    enumFromThen   (CPtrdiff i) (CPtrdiff j)       = map CPtrdiff (enumFromThen i j) ;    enumFromTo     (CPtrdiff i) (CPtrdiff j)       = map CPtrdiff (enumFromTo i j) ;    enumFromThenTo (CPtrdiff i) (CPtrdiff j) (CPtrdiff k) = map CPtrdiff (enumFromThenTo i j k) } ; instance Storable CPtrdiff where {    sizeOf    (CPtrdiff x)       = sizeOf x ;    alignment (CPtrdiff x)       = alignment x ;    peekElemOff a i       = liftM CPtrdiff (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CPtrdiff x) = pokeElemOff (castPtr a) i x } ; tyConCPtrdiff = mkTyCon "CPtrdiff"; instance Typeable CPtrdiff where { typeOf _ = mkTyConApp tyConCPtrdiff [] } ; ; instance Bounded CPtrdiff where {    minBound = CPtrdiff minBound ;    maxBound = CPtrdiff maxBound } ; instance Integral CPtrdiff where {    (CPtrdiff i) `quot`    (CPtrdiff j) = CPtrdiff (i `quot` j) ;    (CPtrdiff i) `rem`     (CPtrdiff j) = CPtrdiff (i `rem`  j) ;    (CPtrdiff i) `div`     (CPtrdiff j) = CPtrdiff (i `div`  j) ;    (CPtrdiff i) `mod`     (CPtrdiff j) = CPtrdiff (i `mod`  j) ;    (CPtrdiff i) `quotRem` (CPtrdiff j) = let (q,r) = i `quotRem` j in (CPtrdiff q, CPtrdiff r) ;    (CPtrdiff i) `divMod`  (CPtrdiff j) = let (d,m) = i `divMod`  j in (CPtrdiff d, CPtrdiff m) ;    toInteger (CPtrdiff i)       = toInteger i } ; instance Bits CPtrdiff where {   (CPtrdiff x) .&.     (CPtrdiff y)   = CPtrdiff (x .&.   y) ;   (CPtrdiff x) .|.     (CPtrdiff y)   = CPtrdiff (x .|.   y) ;   (CPtrdiff x) `xor`   (CPtrdiff y)   = CPtrdiff (x `xor` y) ;   complement    (CPtrdiff x)   = CPtrdiff (complement x) ;   shift         (CPtrdiff x) n = CPtrdiff (shift x n) ;   rotate        (CPtrdiff x) n = CPtrdiff (rotate x n) ;   bit                 n = CPtrdiff (bit n) ;   setBit        (CPtrdiff x) n = CPtrdiff (setBit x n) ;   clearBit      (CPtrdiff x) n = CPtrdiff (clearBit x n) ;   complementBit (CPtrdiff x) n = CPtrdiff (complementBit x n) ;   testBit       (CPtrdiff x) n = testBit x n ;   bitSize       (CPtrdiff x)   = bitSize x ;   isSigned      (CPtrdiff x)   = isSigned x }
-- | Haskell type representing the C @size_t@ type.
newtype CSize = CSize Word64 deriving (Eq, Ord) ; instance Num CSize where {    (CSize i) + (CSize j) = CSize (i + j) ;    (CSize i) - (CSize j) = CSize (i - j) ;    (CSize i) * (CSize j) = CSize (i * j) ;    negate  (CSize i) = CSize (negate i) ;    abs     (CSize i) = CSize (abs    i) ;    signum  (CSize i) = CSize (signum i) ;    fromInteger x = CSize (fromInteger x) } ; instance Real CSize where {    toRational (CSize i) = toRational i } ; instance Read CSize where {    readsPrec p s = map (\(x, t) -> (CSize x, t)) (readsPrec p s) } ; instance Show CSize where {    showsPrec p (CSize x) = showsPrec p x } ; instance Enum CSize where {    succ           (CSize i)             = CSize (succ i) ;    pred           (CSize i)             = CSize (pred i) ;    toEnum               x           = CSize (toEnum x) ;    fromEnum       (CSize i)             = fromEnum i ;    enumFrom       (CSize i)             = map CSize (enumFrom i) ;    enumFromThen   (CSize i) (CSize j)       = map CSize (enumFromThen i j) ;    enumFromTo     (CSize i) (CSize j)       = map CSize (enumFromTo i j) ;    enumFromThenTo (CSize i) (CSize j) (CSize k) = map CSize (enumFromThenTo i j k) } ; instance Storable CSize where {    sizeOf    (CSize x)       = sizeOf x ;    alignment (CSize x)       = alignment x ;    peekElemOff a i       = liftM CSize (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CSize x) = pokeElemOff (castPtr a) i x } ; tyConCSize = mkTyCon "CSize"; instance Typeable CSize where { typeOf _ = mkTyConApp tyConCSize [] } ; ; instance Bounded CSize where {    minBound = CSize minBound ;    maxBound = CSize maxBound } ; instance Integral CSize where {    (CSize i) `quot`    (CSize j) = CSize (i `quot` j) ;    (CSize i) `rem`     (CSize j) = CSize (i `rem`  j) ;    (CSize i) `div`     (CSize j) = CSize (i `div`  j) ;    (CSize i) `mod`     (CSize j) = CSize (i `mod`  j) ;    (CSize i) `quotRem` (CSize j) = let (q,r) = i `quotRem` j in (CSize q, CSize r) ;    (CSize i) `divMod`  (CSize j) = let (d,m) = i `divMod`  j in (CSize d, CSize m) ;    toInteger (CSize i)       = toInteger i } ; instance Bits CSize where {   (CSize x) .&.     (CSize y)   = CSize (x .&.   y) ;   (CSize x) .|.     (CSize y)   = CSize (x .|.   y) ;   (CSize x) `xor`   (CSize y)   = CSize (x `xor` y) ;   complement    (CSize x)   = CSize (complement x) ;   shift         (CSize x) n = CSize (shift x n) ;   rotate        (CSize x) n = CSize (rotate x n) ;   bit                 n = CSize (bit n) ;   setBit        (CSize x) n = CSize (setBit x n) ;   clearBit      (CSize x) n = CSize (clearBit x n) ;   complementBit (CSize x) n = CSize (complementBit x n) ;   testBit       (CSize x) n = testBit x n ;   bitSize       (CSize x)   = bitSize x ;   isSigned      (CSize x)   = isSigned x }
-- | Haskell type representing the C @wchar_t@ type.
newtype CWchar = CWchar Int32 deriving (Eq, Ord) ; instance Num CWchar where {    (CWchar i) + (CWchar j) = CWchar (i + j) ;    (CWchar i) - (CWchar j) = CWchar (i - j) ;    (CWchar i) * (CWchar j) = CWchar (i * j) ;    negate  (CWchar i) = CWchar (negate i) ;    abs     (CWchar i) = CWchar (abs    i) ;    signum  (CWchar i) = CWchar (signum i) ;    fromInteger x = CWchar (fromInteger x) } ; instance Real CWchar where {    toRational (CWchar i) = toRational i } ; instance Read CWchar where {    readsPrec p s = map (\(x, t) -> (CWchar x, t)) (readsPrec p s) } ; instance Show CWchar where {    showsPrec p (CWchar x) = showsPrec p x } ; instance Enum CWchar where {    succ           (CWchar i)             = CWchar (succ i) ;    pred           (CWchar i)             = CWchar (pred i) ;    toEnum               x           = CWchar (toEnum x) ;    fromEnum       (CWchar i)             = fromEnum i ;    enumFrom       (CWchar i)             = map CWchar (enumFrom i) ;    enumFromThen   (CWchar i) (CWchar j)       = map CWchar (enumFromThen i j) ;    enumFromTo     (CWchar i) (CWchar j)       = map CWchar (enumFromTo i j) ;    enumFromThenTo (CWchar i) (CWchar j) (CWchar k) = map CWchar (enumFromThenTo i j k) } ; instance Storable CWchar where {    sizeOf    (CWchar x)       = sizeOf x ;    alignment (CWchar x)       = alignment x ;    peekElemOff a i       = liftM CWchar (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CWchar x) = pokeElemOff (castPtr a) i x } ; tyConCWchar = mkTyCon "CWchar"; instance Typeable CWchar where { typeOf _ = mkTyConApp tyConCWchar [] } ; ; instance Bounded CWchar where {    minBound = CWchar minBound ;    maxBound = CWchar maxBound } ; instance Integral CWchar where {    (CWchar i) `quot`    (CWchar j) = CWchar (i `quot` j) ;    (CWchar i) `rem`     (CWchar j) = CWchar (i `rem`  j) ;    (CWchar i) `div`     (CWchar j) = CWchar (i `div`  j) ;    (CWchar i) `mod`     (CWchar j) = CWchar (i `mod`  j) ;    (CWchar i) `quotRem` (CWchar j) = let (q,r) = i `quotRem` j in (CWchar q, CWchar r) ;    (CWchar i) `divMod`  (CWchar j) = let (d,m) = i `divMod`  j in (CWchar d, CWchar m) ;    toInteger (CWchar i)       = toInteger i } ; instance Bits CWchar where {   (CWchar x) .&.     (CWchar y)   = CWchar (x .&.   y) ;   (CWchar x) .|.     (CWchar y)   = CWchar (x .|.   y) ;   (CWchar x) `xor`   (CWchar y)   = CWchar (x `xor` y) ;   complement    (CWchar x)   = CWchar (complement x) ;   shift         (CWchar x) n = CWchar (shift x n) ;   rotate        (CWchar x) n = CWchar (rotate x n) ;   bit                 n = CWchar (bit n) ;   setBit        (CWchar x) n = CWchar (setBit x n) ;   clearBit      (CWchar x) n = CWchar (clearBit x n) ;   complementBit (CWchar x) n = CWchar (complementBit x n) ;   testBit       (CWchar x) n = testBit x n ;   bitSize       (CWchar x)   = bitSize x ;   isSigned      (CWchar x)   = isSigned x }
-- | Haskell type representing the C @sig_atomic_t@ type.
newtype CSigAtomic = CSigAtomic Int32 deriving (Eq, Ord) ; instance Num CSigAtomic where {    (CSigAtomic i) + (CSigAtomic j) = CSigAtomic (i + j) ;    (CSigAtomic i) - (CSigAtomic j) = CSigAtomic (i - j) ;    (CSigAtomic i) * (CSigAtomic j) = CSigAtomic (i * j) ;    negate  (CSigAtomic i) = CSigAtomic (negate i) ;    abs     (CSigAtomic i) = CSigAtomic (abs    i) ;    signum  (CSigAtomic i) = CSigAtomic (signum i) ;    fromInteger x = CSigAtomic (fromInteger x) } ; instance Real CSigAtomic where {    toRational (CSigAtomic i) = toRational i } ; instance Read CSigAtomic where {    readsPrec p s = map (\(x, t) -> (CSigAtomic x, t)) (readsPrec p s) } ; instance Show CSigAtomic where {    showsPrec p (CSigAtomic x) = showsPrec p x } ; instance Enum CSigAtomic where {    succ           (CSigAtomic i)             = CSigAtomic (succ i) ;    pred           (CSigAtomic i)             = CSigAtomic (pred i) ;    toEnum               x           = CSigAtomic (toEnum x) ;    fromEnum       (CSigAtomic i)             = fromEnum i ;    enumFrom       (CSigAtomic i)             = map CSigAtomic (enumFrom i) ;    enumFromThen   (CSigAtomic i) (CSigAtomic j)       = map CSigAtomic (enumFromThen i j) ;    enumFromTo     (CSigAtomic i) (CSigAtomic j)       = map CSigAtomic (enumFromTo i j) ;    enumFromThenTo (CSigAtomic i) (CSigAtomic j) (CSigAtomic k) = map CSigAtomic (enumFromThenTo i j k) } ; instance Storable CSigAtomic where {    sizeOf    (CSigAtomic x)       = sizeOf x ;    alignment (CSigAtomic x)       = alignment x ;    peekElemOff a i       = liftM CSigAtomic (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CSigAtomic x) = pokeElemOff (castPtr a) i x } ; tyConCSigAtomic = mkTyCon "CSigAtomic"; instance Typeable CSigAtomic where { typeOf _ = mkTyConApp tyConCSigAtomic [] } ; ; instance Bounded CSigAtomic where {    minBound = CSigAtomic minBound ;    maxBound = CSigAtomic maxBound } ; instance Integral CSigAtomic where {    (CSigAtomic i) `quot`    (CSigAtomic j) = CSigAtomic (i `quot` j) ;    (CSigAtomic i) `rem`     (CSigAtomic j) = CSigAtomic (i `rem`  j) ;    (CSigAtomic i) `div`     (CSigAtomic j) = CSigAtomic (i `div`  j) ;    (CSigAtomic i) `mod`     (CSigAtomic j) = CSigAtomic (i `mod`  j) ;    (CSigAtomic i) `quotRem` (CSigAtomic j) = let (q,r) = i `quotRem` j in (CSigAtomic q, CSigAtomic r) ;    (CSigAtomic i) `divMod`  (CSigAtomic j) = let (d,m) = i `divMod`  j in (CSigAtomic d, CSigAtomic m) ;    toInteger (CSigAtomic i)       = toInteger i } ; instance Bits CSigAtomic where {   (CSigAtomic x) .&.     (CSigAtomic y)   = CSigAtomic (x .&.   y) ;   (CSigAtomic x) .|.     (CSigAtomic y)   = CSigAtomic (x .|.   y) ;   (CSigAtomic x) `xor`   (CSigAtomic y)   = CSigAtomic (x `xor` y) ;   complement    (CSigAtomic x)   = CSigAtomic (complement x) ;   shift         (CSigAtomic x) n = CSigAtomic (shift x n) ;   rotate        (CSigAtomic x) n = CSigAtomic (rotate x n) ;   bit                 n = CSigAtomic (bit n) ;   setBit        (CSigAtomic x) n = CSigAtomic (setBit x n) ;   clearBit      (CSigAtomic x) n = CSigAtomic (clearBit x n) ;   complementBit (CSigAtomic x) n = CSigAtomic (complementBit x n) ;   testBit       (CSigAtomic x) n = testBit x n ;   bitSize       (CSigAtomic x)   = bitSize x ;   isSigned      (CSigAtomic x)   = isSigned x }

{-# RULES
"fromIntegral/a->CPtrdiff"   fromIntegral = \x -> CPtrdiff   (fromIntegral x)
"fromIntegral/a->CSize"      fromIntegral = \x -> CSize      (fromIntegral x)
"fromIntegral/a->CWchar"     fromIntegral = \x -> CWchar     (fromIntegral x)
"fromIntegral/a->CSigAtomic" fromIntegral = \x -> CSigAtomic (fromIntegral x)

"fromIntegral/CPtrdiff->a"   fromIntegral = \(CPtrdiff   x) -> fromIntegral x
"fromIntegral/CSize->a"      fromIntegral = \(CSize      x) -> fromIntegral x
"fromIntegral/CWchar->a"     fromIntegral = \(CWchar     x) -> fromIntegral x
"fromIntegral/CSigAtomic->a" fromIntegral = \(CSigAtomic x) -> fromIntegral x
 #-}

-- | Haskell type representing the C @clock_t@ type.
newtype CClock = CClock Int64 deriving (Eq, Ord) ; instance Num CClock where {    (CClock i) + (CClock j) = CClock (i + j) ;    (CClock i) - (CClock j) = CClock (i - j) ;    (CClock i) * (CClock j) = CClock (i * j) ;    negate  (CClock i) = CClock (negate i) ;    abs     (CClock i) = CClock (abs    i) ;    signum  (CClock i) = CClock (signum i) ;    fromInteger x = CClock (fromInteger x) } ; instance Real CClock where {    toRational (CClock i) = toRational i } ; instance Read CClock where {    readsPrec p s = map (\(x, t) -> (CClock x, t)) (readsPrec p s) } ; instance Show CClock where {    showsPrec p (CClock x) = showsPrec p x } ; instance Enum CClock where {    succ           (CClock i)             = CClock (succ i) ;    pred           (CClock i)             = CClock (pred i) ;    toEnum               x           = CClock (toEnum x) ;    fromEnum       (CClock i)             = fromEnum i ;    enumFrom       (CClock i)             = map CClock (enumFrom i) ;    enumFromThen   (CClock i) (CClock j)       = map CClock (enumFromThen i j) ;    enumFromTo     (CClock i) (CClock j)       = map CClock (enumFromTo i j) ;    enumFromThenTo (CClock i) (CClock j) (CClock k) = map CClock (enumFromThenTo i j k) } ; instance Storable CClock where {    sizeOf    (CClock x)       = sizeOf x ;    alignment (CClock x)       = alignment x ;    peekElemOff a i       = liftM CClock (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CClock x) = pokeElemOff (castPtr a) i x } ; tyConCClock = mkTyCon "CClock"; instance Typeable CClock where { typeOf _ = mkTyConApp tyConCClock [] } ;
-- | Haskell type representing the C @time_t@ type.
newtype CTime = CTime Int64 deriving (Eq, Ord) ; instance Num CTime where {    (CTime i) + (CTime j) = CTime (i + j) ;    (CTime i) - (CTime j) = CTime (i - j) ;    (CTime i) * (CTime j) = CTime (i * j) ;    negate  (CTime i) = CTime (negate i) ;    abs     (CTime i) = CTime (abs    i) ;    signum  (CTime i) = CTime (signum i) ;    fromInteger x = CTime (fromInteger x) } ; instance Real CTime where {    toRational (CTime i) = toRational i } ; instance Read CTime where {    readsPrec p s = map (\(x, t) -> (CTime x, t)) (readsPrec p s) } ; instance Show CTime where {    showsPrec p (CTime x) = showsPrec p x } ; instance Enum CTime where {    succ           (CTime i)             = CTime (succ i) ;    pred           (CTime i)             = CTime (pred i) ;    toEnum               x           = CTime (toEnum x) ;    fromEnum       (CTime i)             = fromEnum i ;    enumFrom       (CTime i)             = map CTime (enumFrom i) ;    enumFromThen   (CTime i) (CTime j)       = map CTime (enumFromThen i j) ;    enumFromTo     (CTime i) (CTime j)       = map CTime (enumFromTo i j) ;    enumFromThenTo (CTime i) (CTime j) (CTime k) = map CTime (enumFromThenTo i j k) } ; instance Storable CTime where {    sizeOf    (CTime x)       = sizeOf x ;    alignment (CTime x)       = alignment x ;    peekElemOff a i       = liftM CTime (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CTime x) = pokeElemOff (castPtr a) i x } ; tyConCTime = mkTyCon "CTime"; instance Typeable CTime where { typeOf _ = mkTyConApp tyConCTime [] } ;

-- FIXME: Implement and provide instances for Eq and Storable
-- | Haskell type representing the C @FILE@ type.
data CFile = CFile
-- | Haskell type representing the C @fpos_t@ type.
data CFpos = CFpos
-- | Haskell type representing the C @jmp_buf@ type.
data CJmpBuf = CJmpBuf

newtype CIntPtr = CIntPtr Int64 deriving (Eq, Ord) ; instance Num CIntPtr where {    (CIntPtr i) + (CIntPtr j) = CIntPtr (i + j) ;    (CIntPtr i) - (CIntPtr j) = CIntPtr (i - j) ;    (CIntPtr i) * (CIntPtr j) = CIntPtr (i * j) ;    negate  (CIntPtr i) = CIntPtr (negate i) ;    abs     (CIntPtr i) = CIntPtr (abs    i) ;    signum  (CIntPtr i) = CIntPtr (signum i) ;    fromInteger x = CIntPtr (fromInteger x) } ; instance Real CIntPtr where {    toRational (CIntPtr i) = toRational i } ; instance Read CIntPtr where {    readsPrec p s = map (\(x, t) -> (CIntPtr x, t)) (readsPrec p s) } ; instance Show CIntPtr where {    showsPrec p (CIntPtr x) = showsPrec p x } ; instance Enum CIntPtr where {    succ           (CIntPtr i)             = CIntPtr (succ i) ;    pred           (CIntPtr i)             = CIntPtr (pred i) ;    toEnum               x           = CIntPtr (toEnum x) ;    fromEnum       (CIntPtr i)             = fromEnum i ;    enumFrom       (CIntPtr i)             = map CIntPtr (enumFrom i) ;    enumFromThen   (CIntPtr i) (CIntPtr j)       = map CIntPtr (enumFromThen i j) ;    enumFromTo     (CIntPtr i) (CIntPtr j)       = map CIntPtr (enumFromTo i j) ;    enumFromThenTo (CIntPtr i) (CIntPtr j) (CIntPtr k) = map CIntPtr (enumFromThenTo i j k) } ; instance Storable CIntPtr where {    sizeOf    (CIntPtr x)       = sizeOf x ;    alignment (CIntPtr x)       = alignment x ;    peekElemOff a i       = liftM CIntPtr (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CIntPtr x) = pokeElemOff (castPtr a) i x } ; tyConCIntPtr = mkTyCon "CIntPtr"; instance Typeable CIntPtr where { typeOf _ = mkTyConApp tyConCIntPtr [] } ; ; instance Bounded CIntPtr where {    minBound = CIntPtr minBound ;    maxBound = CIntPtr maxBound } ; instance Integral CIntPtr where {    (CIntPtr i) `quot`    (CIntPtr j) = CIntPtr (i `quot` j) ;    (CIntPtr i) `rem`     (CIntPtr j) = CIntPtr (i `rem`  j) ;    (CIntPtr i) `div`     (CIntPtr j) = CIntPtr (i `div`  j) ;    (CIntPtr i) `mod`     (CIntPtr j) = CIntPtr (i `mod`  j) ;    (CIntPtr i) `quotRem` (CIntPtr j) = let (q,r) = i `quotRem` j in (CIntPtr q, CIntPtr r) ;    (CIntPtr i) `divMod`  (CIntPtr j) = let (d,m) = i `divMod`  j in (CIntPtr d, CIntPtr m) ;    toInteger (CIntPtr i)       = toInteger i } ; instance Bits CIntPtr where {   (CIntPtr x) .&.     (CIntPtr y)   = CIntPtr (x .&.   y) ;   (CIntPtr x) .|.     (CIntPtr y)   = CIntPtr (x .|.   y) ;   (CIntPtr x) `xor`   (CIntPtr y)   = CIntPtr (x `xor` y) ;   complement    (CIntPtr x)   = CIntPtr (complement x) ;   shift         (CIntPtr x) n = CIntPtr (shift x n) ;   rotate        (CIntPtr x) n = CIntPtr (rotate x n) ;   bit                 n = CIntPtr (bit n) ;   setBit        (CIntPtr x) n = CIntPtr (setBit x n) ;   clearBit      (CIntPtr x) n = CIntPtr (clearBit x n) ;   complementBit (CIntPtr x) n = CIntPtr (complementBit x n) ;   testBit       (CIntPtr x) n = testBit x n ;   bitSize       (CIntPtr x)   = bitSize x ;   isSigned      (CIntPtr x)   = isSigned x }
newtype CUIntPtr = CUIntPtr Word64 deriving (Eq, Ord) ; instance Num CUIntPtr where {    (CUIntPtr i) + (CUIntPtr j) = CUIntPtr (i + j) ;    (CUIntPtr i) - (CUIntPtr j) = CUIntPtr (i - j) ;    (CUIntPtr i) * (CUIntPtr j) = CUIntPtr (i * j) ;    negate  (CUIntPtr i) = CUIntPtr (negate i) ;    abs     (CUIntPtr i) = CUIntPtr (abs    i) ;    signum  (CUIntPtr i) = CUIntPtr (signum i) ;    fromInteger x = CUIntPtr (fromInteger x) } ; instance Real CUIntPtr where {    toRational (CUIntPtr i) = toRational i } ; instance Read CUIntPtr where {    readsPrec p s = map (\(x, t) -> (CUIntPtr x, t)) (readsPrec p s) } ; instance Show CUIntPtr where {    showsPrec p (CUIntPtr x) = showsPrec p x } ; instance Enum CUIntPtr where {    succ           (CUIntPtr i)             = CUIntPtr (succ i) ;    pred           (CUIntPtr i)             = CUIntPtr (pred i) ;    toEnum               x           = CUIntPtr (toEnum x) ;    fromEnum       (CUIntPtr i)             = fromEnum i ;    enumFrom       (CUIntPtr i)             = map CUIntPtr (enumFrom i) ;    enumFromThen   (CUIntPtr i) (CUIntPtr j)       = map CUIntPtr (enumFromThen i j) ;    enumFromTo     (CUIntPtr i) (CUIntPtr j)       = map CUIntPtr (enumFromTo i j) ;    enumFromThenTo (CUIntPtr i) (CUIntPtr j) (CUIntPtr k) = map CUIntPtr (enumFromThenTo i j k) } ; instance Storable CUIntPtr where {    sizeOf    (CUIntPtr x)       = sizeOf x ;    alignment (CUIntPtr x)       = alignment x ;    peekElemOff a i       = liftM CUIntPtr (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CUIntPtr x) = pokeElemOff (castPtr a) i x } ; tyConCUIntPtr = mkTyCon "CUIntPtr"; instance Typeable CUIntPtr where { typeOf _ = mkTyConApp tyConCUIntPtr [] } ; ; instance Bounded CUIntPtr where {    minBound = CUIntPtr minBound ;    maxBound = CUIntPtr maxBound } ; instance Integral CUIntPtr where {    (CUIntPtr i) `quot`    (CUIntPtr j) = CUIntPtr (i `quot` j) ;    (CUIntPtr i) `rem`     (CUIntPtr j) = CUIntPtr (i `rem`  j) ;    (CUIntPtr i) `div`     (CUIntPtr j) = CUIntPtr (i `div`  j) ;    (CUIntPtr i) `mod`     (CUIntPtr j) = CUIntPtr (i `mod`  j) ;    (CUIntPtr i) `quotRem` (CUIntPtr j) = let (q,r) = i `quotRem` j in (CUIntPtr q, CUIntPtr r) ;    (CUIntPtr i) `divMod`  (CUIntPtr j) = let (d,m) = i `divMod`  j in (CUIntPtr d, CUIntPtr m) ;    toInteger (CUIntPtr i)       = toInteger i } ; instance Bits CUIntPtr where {   (CUIntPtr x) .&.     (CUIntPtr y)   = CUIntPtr (x .&.   y) ;   (CUIntPtr x) .|.     (CUIntPtr y)   = CUIntPtr (x .|.   y) ;   (CUIntPtr x) `xor`   (CUIntPtr y)   = CUIntPtr (x `xor` y) ;   complement    (CUIntPtr x)   = CUIntPtr (complement x) ;   shift         (CUIntPtr x) n = CUIntPtr (shift x n) ;   rotate        (CUIntPtr x) n = CUIntPtr (rotate x n) ;   bit                 n = CUIntPtr (bit n) ;   setBit        (CUIntPtr x) n = CUIntPtr (setBit x n) ;   clearBit      (CUIntPtr x) n = CUIntPtr (clearBit x n) ;   complementBit (CUIntPtr x) n = CUIntPtr (complementBit x n) ;   testBit       (CUIntPtr x) n = testBit x n ;   bitSize       (CUIntPtr x)   = bitSize x ;   isSigned      (CUIntPtr x)   = isSigned x }
newtype CIntMax = CIntMax Int64 deriving (Eq, Ord) ; instance Num CIntMax where {    (CIntMax i) + (CIntMax j) = CIntMax (i + j) ;    (CIntMax i) - (CIntMax j) = CIntMax (i - j) ;    (CIntMax i) * (CIntMax j) = CIntMax (i * j) ;    negate  (CIntMax i) = CIntMax (negate i) ;    abs     (CIntMax i) = CIntMax (abs    i) ;    signum  (CIntMax i) = CIntMax (signum i) ;    fromInteger x = CIntMax (fromInteger x) } ; instance Real CIntMax where {    toRational (CIntMax i) = toRational i } ; instance Read CIntMax where {    readsPrec p s = map (\(x, t) -> (CIntMax x, t)) (readsPrec p s) } ; instance Show CIntMax where {    showsPrec p (CIntMax x) = showsPrec p x } ; instance Enum CIntMax where {    succ           (CIntMax i)             = CIntMax (succ i) ;    pred           (CIntMax i)             = CIntMax (pred i) ;    toEnum               x           = CIntMax (toEnum x) ;    fromEnum       (CIntMax i)             = fromEnum i ;    enumFrom       (CIntMax i)             = map CIntMax (enumFrom i) ;    enumFromThen   (CIntMax i) (CIntMax j)       = map CIntMax (enumFromThen i j) ;    enumFromTo     (CIntMax i) (CIntMax j)       = map CIntMax (enumFromTo i j) ;    enumFromThenTo (CIntMax i) (CIntMax j) (CIntMax k) = map CIntMax (enumFromThenTo i j k) } ; instance Storable CIntMax where {    sizeOf    (CIntMax x)       = sizeOf x ;    alignment (CIntMax x)       = alignment x ;    peekElemOff a i       = liftM CIntMax (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CIntMax x) = pokeElemOff (castPtr a) i x } ; tyConCIntMax = mkTyCon "CIntMax"; instance Typeable CIntMax where { typeOf _ = mkTyConApp tyConCIntMax [] } ; ; instance Bounded CIntMax where {    minBound = CIntMax minBound ;    maxBound = CIntMax maxBound } ; instance Integral CIntMax where {    (CIntMax i) `quot`    (CIntMax j) = CIntMax (i `quot` j) ;    (CIntMax i) `rem`     (CIntMax j) = CIntMax (i `rem`  j) ;    (CIntMax i) `div`     (CIntMax j) = CIntMax (i `div`  j) ;    (CIntMax i) `mod`     (CIntMax j) = CIntMax (i `mod`  j) ;    (CIntMax i) `quotRem` (CIntMax j) = let (q,r) = i `quotRem` j in (CIntMax q, CIntMax r) ;    (CIntMax i) `divMod`  (CIntMax j) = let (d,m) = i `divMod`  j in (CIntMax d, CIntMax m) ;    toInteger (CIntMax i)       = toInteger i } ; instance Bits CIntMax where {   (CIntMax x) .&.     (CIntMax y)   = CIntMax (x .&.   y) ;   (CIntMax x) .|.     (CIntMax y)   = CIntMax (x .|.   y) ;   (CIntMax x) `xor`   (CIntMax y)   = CIntMax (x `xor` y) ;   complement    (CIntMax x)   = CIntMax (complement x) ;   shift         (CIntMax x) n = CIntMax (shift x n) ;   rotate        (CIntMax x) n = CIntMax (rotate x n) ;   bit                 n = CIntMax (bit n) ;   setBit        (CIntMax x) n = CIntMax (setBit x n) ;   clearBit      (CIntMax x) n = CIntMax (clearBit x n) ;   complementBit (CIntMax x) n = CIntMax (complementBit x n) ;   testBit       (CIntMax x) n = testBit x n ;   bitSize       (CIntMax x)   = bitSize x ;   isSigned      (CIntMax x)   = isSigned x }
newtype CUIntMax = CUIntMax Word64 deriving (Eq, Ord) ; instance Num CUIntMax where {    (CUIntMax i) + (CUIntMax j) = CUIntMax (i + j) ;    (CUIntMax i) - (CUIntMax j) = CUIntMax (i - j) ;    (CUIntMax i) * (CUIntMax j) = CUIntMax (i * j) ;    negate  (CUIntMax i) = CUIntMax (negate i) ;    abs     (CUIntMax i) = CUIntMax (abs    i) ;    signum  (CUIntMax i) = CUIntMax (signum i) ;    fromInteger x = CUIntMax (fromInteger x) } ; instance Real CUIntMax where {    toRational (CUIntMax i) = toRational i } ; instance Read CUIntMax where {    readsPrec p s = map (\(x, t) -> (CUIntMax x, t)) (readsPrec p s) } ; instance Show CUIntMax where {    showsPrec p (CUIntMax x) = showsPrec p x } ; instance Enum CUIntMax where {    succ           (CUIntMax i)             = CUIntMax (succ i) ;    pred           (CUIntMax i)             = CUIntMax (pred i) ;    toEnum               x           = CUIntMax (toEnum x) ;    fromEnum       (CUIntMax i)             = fromEnum i ;    enumFrom       (CUIntMax i)             = map CUIntMax (enumFrom i) ;    enumFromThen   (CUIntMax i) (CUIntMax j)       = map CUIntMax (enumFromThen i j) ;    enumFromTo     (CUIntMax i) (CUIntMax j)       = map CUIntMax (enumFromTo i j) ;    enumFromThenTo (CUIntMax i) (CUIntMax j) (CUIntMax k) = map CUIntMax (enumFromThenTo i j k) } ; instance Storable CUIntMax where {    sizeOf    (CUIntMax x)       = sizeOf x ;    alignment (CUIntMax x)       = alignment x ;    peekElemOff a i       = liftM CUIntMax (peekElemOff (castPtr a) i) ;    pokeElemOff a i (CUIntMax x) = pokeElemOff (castPtr a) i x } ; tyConCUIntMax = mkTyCon "CUIntMax"; instance Typeable CUIntMax where { typeOf _ = mkTyConApp tyConCUIntMax [] } ; ; instance Bounded CUIntMax where {    minBound = CUIntMax minBound ;    maxBound = CUIntMax maxBound } ; instance Integral CUIntMax where {    (CUIntMax i) `quot`    (CUIntMax j) = CUIntMax (i `quot` j) ;    (CUIntMax i) `rem`     (CUIntMax j) = CUIntMax (i `rem`  j) ;    (CUIntMax i) `div`     (CUIntMax j) = CUIntMax (i `div`  j) ;    (CUIntMax i) `mod`     (CUIntMax j) = CUIntMax (i `mod`  j) ;    (CUIntMax i) `quotRem` (CUIntMax j) = let (q,r) = i `quotRem` j in (CUIntMax q, CUIntMax r) ;    (CUIntMax i) `divMod`  (CUIntMax j) = let (d,m) = i `divMod`  j in (CUIntMax d, CUIntMax m) ;    toInteger (CUIntMax i)       = toInteger i } ; instance Bits CUIntMax where {   (CUIntMax x) .&.     (CUIntMax y)   = CUIntMax (x .&.   y) ;   (CUIntMax x) .|.     (CUIntMax y)   = CUIntMax (x .|.   y) ;   (CUIntMax x) `xor`   (CUIntMax y)   = CUIntMax (x `xor` y) ;   complement    (CUIntMax x)   = CUIntMax (complement x) ;   shift         (CUIntMax x) n = CUIntMax (shift x n) ;   rotate        (CUIntMax x) n = CUIntMax (rotate x n) ;   bit                 n = CUIntMax (bit n) ;   setBit        (CUIntMax x) n = CUIntMax (setBit x n) ;   clearBit      (CUIntMax x) n = CUIntMax (clearBit x n) ;   complementBit (CUIntMax x) n = CUIntMax (complementBit x n) ;   testBit       (CUIntMax x) n = testBit x n ;   bitSize       (CUIntMax x)   = bitSize x ;   isSigned      (CUIntMax x)   = isSigned x }

{-# RULES
"fromIntegral/a->CIntPtr"  fromIntegral = \x -> CIntPtr  (fromIntegral x)
"fromIntegral/a->CUIntPtr" fromIntegral = \x -> CUIntPtr (fromIntegral x)
"fromIntegral/a->CIntMax"  fromIntegral = \x -> CIntMax  (fromIntegral x)
"fromIntegral/a->CUIntMax" fromIntegral = \x -> CUIntMax (fromIntegral x)
 #-}

-- C99 types which are still missing include:
-- wint_t, wctrans_t, wctype_t

{- $ctypes

These types are needed to accurately represent C function prototypes,
in order to access C library interfaces in Haskell.  The Haskell system
is not required to represent those types exactly as C does, but the
following guarantees are provided concerning a Haskell type @CT@
representing a C type @t@:

* If a C function prototype has @t@ as an argument or result type, the
  use of @CT@ in the corresponding position in a foreign declaration
  permits the Haskell program to access the full range of values encoded
  by the C type; and conversely, any Haskell value for @CT@ has a valid
  representation in C.

* @'sizeOf' ('Prelude.undefined' :: CT)@ will yield the same value as
  @sizeof (t)@ in C.

* @'alignment' ('Prelude.undefined' :: CT)@ matches the alignment
  constraint enforced by the C implementation for @t@.

* The members 'peek' and 'poke' of the 'Storable' class map all values
  of @CT@ to the corresponding value of @t@ and vice versa.

* When an instance of 'Prelude.Bounded' is defined for @CT@, the values
  of 'Prelude.minBound' and 'Prelude.maxBound' coincide with @t_MIN@
  and @t_MAX@ in C.

* When an instance of 'Prelude.Eq' or 'Prelude.Ord' is defined for @CT@,
  the predicates defined by the type class implement the same relation
  as the corresponding predicate in C on @t@.

* When an instance of 'Prelude.Num', 'Prelude.Read', 'Prelude.Integral',
  'Prelude.Fractional', 'Prelude.Floating', 'Prelude.RealFrac', or
  'Prelude.RealFloat' is defined for @CT@, the arithmetic operations
  defined by the type class implement the same function as the
  corresponding arithmetic operations (if available) in C on @t@.

* When an instance of 'Bits' is defined for @CT@, the bitwise operation
  defined by the type class implement the same function as the
  corresponding bitwise operation in C on @t@.

-}

















































