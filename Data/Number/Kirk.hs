
{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Number.Kirk (
  KirkBoundT(..),
  KirkApxT(..),
  KirkSeq0Idx(..),
  KirkReal,KirkImportReal,approx,
  KirkFun10,kirk10,
  KirkFun11,kirk11,
  KirkFun12,kirk12,
  KirkRealPtr,
) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Int
import Data.Word
import Foreign.Storable
import Data.Number.MPFR
import Data.Number.MPFR.FFIhelper (peekP)
import Data.Number.MPFR.Instances.Near ()
import Foreign.Marshal.Utils

#if KIRK_BOUND_SIZE_GMP-0
# error no support for generic GMP type
#endif

type KirkRealPtr = Ptr KirkRealT

data KirkBoundT = KirkBoundT
  { k_exponent :: Int64
  , k_mantissa :: Word64
  }

data KirkApxT = KirkApxT
  { radius :: KirkBoundT
  , center :: MPFR
  }

data KirkSeq0Idx = AbsAcc Int32
                 | Effort Word32

data KirkRealT {-= KirkRealT
  { clazz :: Ptr KirkRealClassT
  }-}

data KirkReal = KirkReal (ForeignPtr KirkRealT)

class KirkImportReal a where
  approx :: a -> KirkSeq0Idx -> IO KirkApxT

instance Show KirkApxT where
  show (KirkApxT (KirkBoundT e m) c) =
    "[" ++ show c ++ " +/- " ++ show m ++ "*2^(" ++ show e ++ ")]"

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

foreign import ccall "kirk_real_ref"     kirk_real_ref     :: KirkRealPtr -> IO KirkRealPtr
--foreign import ccall "kirk_real_unref"   kirk_real_unref   :: KirkRealPtr -> IO ()
foreign import ccall "kirk_real_apx_abs" kirk_real_apx_abs :: KirkRealPtr -> Ptr KirkApxT -> Int32 -> IO ()
foreign import ccall "kirk_real_apx_eff" kirk_real_apx_eff :: KirkRealPtr -> Ptr KirkApxT -> Word32 -> IO ()

foreign import ccall "kirk_apx_init"     kirk_apx_init     :: Ptr KirkApxT -> IO ()
--foreign import ccall "kirk_apx_init2"    kirk_apx_init2    :: Ptr KirkApxT -> mpfr_prec_t -> IO ()
foreign import ccall "kirk_apx_cpy"      kirk_apx_cpy      :: Ptr KirkApxT -> Ptr KirkApxT -> IO ()
--foreign import ccall "kirk_apx_set"      kirk_apx_set      :: Ptr KirkApxT -> Ptr KirkApxT -> IO ()
foreign import ccall "kirk_apx_fini"     kirk_apx_fini     :: Ptr KirkApxT -> IO ()

--foreign import ccall "kirk_real_sv_create" kirk_real_sv_create :: KirkRealPtr -> IO KirkRealPtr

--foreign import ccall "&kirk_apx_fini"    p_kirk_apx_fini   :: FunPtr (Ptr KirkApxT -> IO ())
foreign import ccall "&kirk_real_unref"  p_kirk_real_unref :: FunPtr (KirkRealPtr -> IO ())

foreign import ccall "kirk_real_hs_create" kirk_real_hs_create
  :: FunPtr (Ptr KirkApxT -> Int32 -> IO ())
  -> FunPtr (Ptr KirkApxT -> Word32 -> IO ())
  -> IO KirkRealPtr

foreign import ccall "wrapper" wrap_apx_abs
  :: (Ptr KirkApxT -> Int32 -> IO ())
  -> IO (FunPtr (Ptr KirkApxT -> Int32 -> IO ()))

foreign import ccall "wrapper" wrap_apx_eff
  :: (Ptr KirkApxT -> Word32 -> IO ())
  -> IO (FunPtr (Ptr KirkApxT -> Word32 -> IO ()))


copy_apx :: Ptr KirkApxT -> KirkApxT -> IO ()
copy_apx p_tgt src = with src $ kirk_apx_cpy p_tgt

makeExportReal :: KirkImportReal hr => hr -> IO (ForeignPtr KirkRealT)
makeExportReal r = do
  acc_f <- wrap_apx_abs $ \ptr acc -> approx r (AbsAcc acc) >>= copy_apx ptr
  eff_f <- wrap_apx_eff $ \ptr eff -> approx r (Effort eff) >>= copy_apx ptr
  kirk_real_hs_create acc_f eff_f >>= ref_real_ptr

type KirkFun10 = KirkRealPtr
type KirkFun11 = KirkRealPtr -> IO KirkRealPtr
type KirkFun12 = KirkRealPtr -> KirkRealPtr -> IO KirkRealPtr

kirk10 :: KirkFun10 -> IO KirkReal
kirk10 r = ref_real_ptr r >>= return . KirkReal

kirk11 :: KirkImportReal a => KirkFun11 -> (a -> IO KirkReal)
kirk11 f =
  \p -> do
    fp <- makeExportReal p
    withForeignPtr fp f >>= kirk10

kirk12 :: (KirkImportReal a, KirkImportReal b)
       => KirkFun12 -> (a -> b -> IO KirkReal)
kirk12 f =
  \p q -> do
    fp <- makeExportReal p
    withForeignPtr fp $ \gp -> kirk11 (f gp) q


{-data KirkRealClassT = KirkRealClassT
  { ref   :: FunPtr (Ptr KirkRealT -> IO (Ptr KirkRealT))
  , unref :: FunPtr (Ptr KirkRealT -> IO ())
  , apx_abs :: FunPtr (Ptr KirkRealT -> Ptr KirkApxT -> Int32 -> IO ())
  , apx_eff :: FunPtr (Ptr KirkRealT -> Ptr KirkApxT -> Word32 -> IO ())
  }-}

do_real_apx :: Ptr KirkRealT -> Ptr KirkApxT -> KirkSeq0Idx -> IO ()
do_real_apx real apx (AbsAcc acc) = kirk_real_apx_abs real apx acc
do_real_apx real apx (Effort eff) = kirk_real_apx_eff real apx eff

instance KirkImportReal KirkReal where
  approx (KirkReal real) idx =
    withForeignPtr real $ \r ->
      with (KirkApxT (KirkBoundT 0 0) 0) $ \p -> do
        --kirk_apx_init p
        do_real_apx r p idx
        q <- peek p
        --kirk_apx_fini p
        return q

class KirkRealObj a where
  ref :: Ptr a -> IO (Ptr a)
  unref :: FunPtr (Ptr a -> IO ())

ref_real_ptr :: KirkRealObj a => Ptr a -> IO (ForeignPtr a)
ref_real_ptr r = ref r >>= newForeignPtr unref

instance KirkRealObj KirkRealT where
  ref = kirk_real_ref
  unref = p_kirk_real_unref

{-
-- implemented by kirk_hs_real_t derived from kirk_real_obj_t
-- -> ref/unref taken care of
-- -> apx_abs/apx_eff/destroy left
class (KirkImportReal a) => KirkExportReal a where
  finalize :: a -> IO ()
-}

{- functional approximations -}
{-
instance KirkImportReal KirkRealSV where
  approx = approx

instance KirkImportRealSV KirkRealSV where
--  approx_sv (KirkRealSV fptr) idx = unsafePerformIO $ approx (KirkReal fptr) idx
  approx_sv r i = unsafePerformIO $ approx r i

newtype KirkRealSV = KirkRealSV KirkReal

class (KirkImportReal a) => KirkImportRealSV a where
  approx_sv :: a -> KirkSeq0Idx -> KirkApxT
-}
