
{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Number.Kirk where

import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Int
import Data.Word
import Foreign.Storable
import Data.Number.MPFR
import Data.Number.MPFR.FFIhelper (peekP)
import Data.Number.MPFR.Instances.Near ()
import Foreign.Marshal.Utils

data KirkBoundT = KirkBoundT
  { k_exponent :: Int64
  , k_mantissa :: Word64
  }

instance Storable KirkBoundT where
  sizeOf _    = 16
  alignment _ =  8
  peek ptr    = do
    e <- peekByteOff ptr 0
    m <- peekByteOff ptr 8
    return (KirkBoundT e m)
  poke ptr (KirkBoundT e m) = do
    pokeByteOff ptr 0 e
    pokeByteOff ptr 8 m

data KirkApxT = KirkApxT
  { radius :: KirkBoundT
  , center :: MPFR
  }

instance Storable KirkApxT where
  sizeOf _    = 48
  alignment _ =  8
  peek ptr    = do
    r        <- peekByteOff ptr 0
    p_limbs  <- peekByteOff ptr 40
    fp_limbs <- newForeignPtr_ p_limbs
    c        <- peekP (ptr `plusPtr` 16) fp_limbs
    return (KirkApxT r c)
  poke ptr (KirkApxT r c) = do
    pokeByteOff ptr 0 r
    pokeByteOff ptr 16 c

--foreign import ccall "kirk_real_ref"     kirk_real_ref     :: Ptr KirkRealT -> IO (Ptr KirkRealT)
--foreign import ccall "kirk_real_unref"   kirk_real_unref   :: Ptr KirkRealT -> IO ()
foreign import ccall "kirk_real_apx_abs" kirk_real_apx_abs :: Ptr KirkRealT -> Ptr KirkApxT -> Int32 -> IO ()
foreign import ccall "kirk_real_apx_eff" kirk_real_apx_eff :: Ptr KirkRealT -> Ptr KirkApxT -> Word32 -> IO ()

foreign import ccall "kirk_apx_init"     kirk_apx_init     :: Ptr KirkApxT -> IO ()
--foreign import ccall "kirk_apx_init2"    kirk_apx_init     :: Ptr KirkApxT -> IO ()
foreign import ccall "kirk_apx_fini"     kirk_apx_fini     :: Ptr KirkApxT -> IO ()

--foreign import ccall "&kirk_apx_fini"    p_kirk_apx_fini   :: FunPtr (Ptr KirkApxT -> IO ())
foreign import ccall "&kirk_real_unref"  p_kirk_real_unref :: FunPtr (Ptr KirkRealT -> IO ())

get_real_ptr :: IO (Ptr KirkRealT) -> IO (ForeignPtr KirkRealT)
get_real_ptr get_real = get_real >>= newForeignPtr p_kirk_real_unref

do_real_apx_abs :: Ptr KirkRealT -> Int32 -> IO KirkApxT
do_real_apx_abs real accuracy =
  with (KirkApxT (KirkBoundT 0 0) 0) $ \p -> do
    kirk_apx_init p
    kirk_real_apx_abs real p accuracy
    q <- peek p
    kirk_apx_fini p
    return q

{-data KirkRealClassT = KirkRealClassT
  { ref   :: FunPtr (Ptr KirkRealT -> IO (Ptr KirkRealT))
  , unref :: FunPtr (Ptr KirkRealT -> IO ())
  , apx_abs :: FunPtr (Ptr KirkRealT -> Ptr KirkApxT -> Int32 -> IO ())
  , apx_eff :: FunPtr (Ptr KirkRealT -> Ptr KirkApxT -> Word32 -> IO ())
  }-}

data KirkRealT {-= KirkRealT
  { clazz :: Ptr KirkRealClassT
  }-}
