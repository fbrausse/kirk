
{-# LANGUAGE ForeignFunctionInterface #-}

#if KIRK_BOUND_SIZE_GMP-0
# error no support for generic GMP type
#endif

module Data.Number.Kirk (
  KirkBoundT(..),
  KirkApxT(..),
  KirkSeq0Idx(..),
  KirkReal,KirkImportReal(..),
  KirkFun10,kirk10,
  KirkFun11,kirk11,
  KirkFun12,kirk12,
  KirkRealPtr,
) where

import Foreign.Ptr
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr,withForeignPtr)
import Foreign.StablePtr
import Data.Int (Int32,Int64)
import Data.Word (Word32,Word64)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Utils (with)

#if defined(KIRK_HAVE_HMPFR)

import Foreign.ForeignPtr (newForeignPtr_)
import qualified Data.Number.MPFR (MPFR)
import Data.Number.MPFR.FFIhelper (peekP)
import Data.Number.MPFR.Instances.Near ()
type KirkMPFR = Data.Number.MPFR.MPFR

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

type Approximation = KirkApxT

instance Show KirkApxT where
  show (KirkApxT (KirkBoundT e m) c) =
    "[" ++ show c ++ " +/- " ++ show m ++ "*2^(" ++ show (e-64) ++ ")]"

instance KirkImportReal KirkReal where
  approx (KirkReal real) idx =
    withForeignPtr real $ \r ->
      with (KirkApxT (KirkBoundT 0 0) 0) $ \p -> do
{- TODO:
   this assumes p points to a kirk_apx_t that was initialized the same way
   kirk_apx_init would have -}
        do_real_apx r p idx
        q <- peek p
        return q

copy_apx :: Ptr KirkApxT -> KirkApxT -> IO ()
copy_apx = copy_apx'

#elif defined(KIRK_HAVE_ROUNDED)

import Numeric.MPFR.Types (MPFR(..))
import Numeric.Rounded.Simple (Rounded,show',withInRounded,peekRounded)

type KirkMPFR = Numeric.MPFR.Types.MPFR

instance Storable KirkApxT where
  sizeOf _    = 48
  alignment _ =  8
  peek ptr    = do
    r <- peekByteOff ptr 0
    c <- peekByteOff ptr 16
    return $ KirkApxT r c
  poke ptr (KirkApxT r c) = do
    pokeByteOff ptr 0 r
    pokeByteOff ptr 16 c

foreign import ccall "kirk_apx_init"     kirk_apx_init     :: Ptr KirkApxT -> IO ()
foreign import ccall "kirk_apx_fini"     kirk_apx_fini     :: Ptr KirkApxT -> IO ()

data Approximation = Approximation KirkBoundT Rounded

instance KirkImportReal KirkReal where
  approx (KirkReal real) idx =
    withForeignPtr real $ \r ->
      with (KirkApxT (KirkBoundT 0 0) MPFR { mpfrPrec = 0, mpfrSign = 0, mpfrExp = 0, mpfrD = nullPtr } ) $ \p -> do
        kirk_apx_init p
        do_real_apx r p idx
        (KirkApxT bnd mpfr) <- peek p
        rnd <- with mpfr peekRounded
        kirk_apx_fini p
        return $ Approximation bnd rnd

instance Show Approximation where
  show (Approximation (KirkBoundT e m) c) =
    "[" ++ show' c ++ " +/- " ++ show m ++ "*2^(" ++ show (e-64) ++ ")]"

copy_apx :: Ptr KirkApxT -> Approximation -> IO ()
copy_apx ptr (Approximation bnd rnd) =
  withInRounded rnd $ \mpfr_ptr -> do
    mpfr <- peek mpfr_ptr
    copy_apx' ptr $ KirkApxT bnd mpfr

#endif

type KirkRealPtr = Ptr KirkRealT

data KirkBoundT = KirkBoundT
  { exponent :: Int64
  , mantissa :: Word64
  }

data KirkApxT = KirkApxT
  { radius :: KirkBoundT
  , center :: KirkMPFR
  }

data KirkSeq0Idx = AbsAcc Int32
                 | Effort Word32

data KirkRealT {-= KirkRealT
  { clazz :: Ptr KirkRealClassT
  }-}

data KirkReal = KirkReal (ForeignPtr KirkRealT)

class KirkImportReal a where
  approx :: a -> KirkSeq0Idx -> IO Approximation

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

class KirkRealObj a where
  ref :: Ptr a -> IO (Ptr a)
  unref :: FunPtr (Ptr a -> IO ())

ref_real_ptr :: KirkRealObj a => Ptr a -> IO (ForeignPtr a)
ref_real_ptr r = ref r >>= newForeignPtr unref

--foreign import ccall "kirk_apx_init2"    kirk_apx_init2    :: Ptr KirkApxT -> mpfr_prec_t -> IO ()
foreign import ccall "kirk_apx_cpy"      kirk_apx_cpy      :: Ptr KirkApxT -> Ptr KirkApxT -> IO ()
--foreign import ccall "kirk_apx_set"      kirk_apx_set      :: Ptr KirkApxT -> Ptr KirkApxT -> IO ()
--foreign import ccall "&kirk_apx_fini"    p_kirk_apx_fini   :: FunPtr (Ptr KirkApxT -> IO ())

copy_apx' :: Ptr KirkApxT -> KirkApxT -> IO ()
copy_apx' p_tgt src = with src $ kirk_apx_cpy p_tgt

type KirkHsP = StablePtr (KirkSeq0Idx -> IO Approximation)

kirk_hs_approx :: KirkSeq0Idx -> KirkHsP -> Ptr KirkApxT -> IO ()
kirk_hs_approx idx pr p_tgt = do
  f <- deRefStablePtr pr
  f idx >>= copy_apx p_tgt

foreign export ccall kirk_hs_approx_abs :: Int32 -> KirkHsP -> Ptr KirkApxT -> IO ()
kirk_hs_approx_abs :: Int32 -> KirkHsP -> Ptr KirkApxT -> IO ()
kirk_hs_approx_abs acc = kirk_hs_approx $ AbsAcc acc

foreign export ccall kirk_hs_approx_eff :: Word32 -> KirkHsP -> Ptr KirkApxT -> IO ()
kirk_hs_approx_eff :: Word32 -> KirkHsP -> Ptr KirkApxT -> IO ()
kirk_hs_approx_eff eff = kirk_hs_approx $ Effort eff

foreign import ccall "" kirk_real_hs_create :: KirkHsP -> IO KirkRealPtr

makeHsReal :: KirkImportReal hr => hr -> IO (Ptr KirkRealT)
makeHsReal r = newStablePtr (approx r) >>= kirk_real_hs_create

type KirkFun10 = KirkRealPtr
type KirkFun11 = KirkRealPtr -> IO KirkRealPtr
type KirkFun12 = KirkRealPtr -> KirkRealPtr -> IO KirkRealPtr

kirk10 :: KirkFun10 -> IO KirkReal
kirk10 r = ref_real_ptr r >>= return . KirkReal

kirk11 :: KirkImportReal a => KirkFun11 -> (a -> IO KirkReal)
kirk11 f =
  \p -> do
    fp <- makeHsReal p >>= ref_real_ptr
    withForeignPtr fp f >>= kirk10

kirk12 :: (KirkImportReal a, KirkImportReal b)
       => KirkFun12 -> (a -> b -> IO KirkReal)
kirk12 f =
  \p q -> do
    fp <- makeHsReal p >>= ref_real_ptr
    withForeignPtr fp $ \gp -> kirk11 (f gp) q

foreign import ccall "kirk_real_apx_abs" kirk_real_apx_abs :: KirkRealPtr -> Ptr KirkApxT -> Int32 -> IO ()
foreign import ccall "kirk_real_apx_eff" kirk_real_apx_eff :: KirkRealPtr -> Ptr KirkApxT -> Word32 -> IO ()

do_real_apx :: Ptr KirkRealT -> Ptr KirkApxT -> KirkSeq0Idx -> IO ()
do_real_apx real apx (AbsAcc acc) = kirk_real_apx_abs real apx acc
do_real_apx real apx (Effort eff) = kirk_real_apx_eff real apx eff

foreign import ccall "kirk_real_ref"     kirk_real_ref     :: KirkRealPtr -> IO KirkRealPtr
foreign import ccall "&kirk_real_unref"  p_kirk_real_unref :: FunPtr (KirkRealPtr -> IO ())

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

--foreign import ccall "kirk_real_sv_create" kirk_real_sv_create :: KirkRealPtr -> IO KirkRealPtr

instance KirkImportRealSV KirkRealSV where
--  approx_sv (KirkRealSV fptr) idx = unsafePerformIO $ approx (KirkReal fptr) idx
  approx_sv r i = unsafePerformIO $ approx r i

newtype KirkRealSV = KirkRealSV KirkReal

class (KirkImportReal a) => KirkImportRealSV a where
  approx_sv :: a -> KirkSeq0Idx -> KirkApxT
-}
