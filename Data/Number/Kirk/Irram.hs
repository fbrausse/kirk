
{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Number.Kirk.Irram where

import Data.Number.Kirk
import Foreign.Ptr
import Foreign.C.Types

#ifdef KIRK_HAVE_IRRAM
foreign import ccall "kirk-irram-api.h" kirk_irram_add     :: KirkFun12
foreign import ccall "kirk-irram-api.h" kirk_irram_neg     :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_sub     :: KirkFun12
foreign import ccall "kirk-irram-api.h" kirk_irram_mul     :: KirkFun12
foreign import ccall "kirk-irram-api.h" kirk_irram_inv     :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_div     :: KirkFun12
foreign import ccall "kirk-irram-api.h" kirk_irram_pi      :: KirkFun10
foreign import ccall "kirk-irram-api.h" kirk_irram_e       :: KirkFun10
foreign import ccall "kirk-irram-api.h" kirk_irram_ln2     :: KirkFun10
foreign import ccall "kirk-irram-api.h" kirk_irram_exp     :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_pow     :: Ptr KirkRealT -> CInt -> IO (Ptr KirkRealT)
foreign import ccall "kirk-irram-api.h" kirk_irram_pow_r   :: KirkFun12
foreign import ccall "kirk-irram-api.h" kirk_irram_log     :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_sqrt    :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_root    :: Ptr KirkRealT -> CInt -> IO (Ptr KirkRealT)
foreign import ccall "kirk-irram-api.h" kirk_irram_max     :: KirkFun12
foreign import ccall "kirk-irram-api.h" kirk_irram_min     :: KirkFun12
foreign import ccall "kirk-irram-api.h" kirk_irram_mod     :: KirkFun12
foreign import ccall "kirk-irram-api.h" kirk_irram_sin     :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_cos     :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_tan     :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_sec     :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_cotan   :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_cosec   :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_asin    :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_acos    :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_atan    :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_asec    :: KirkFun11
--KIRK_API kirk_real_t * kirk_irram_acotan (kirk_real_t *);
foreign import ccall "kirk-irram-api.h" kirk_irram_acosec  :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_sinh    :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_cosh    :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_tanh    :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_sech    :: KirkFun11
--KIRK_API kirk_real_t * kirk_irram_cotanh (kirk_real_t *);
foreign import ccall "kirk-irram-api.h" kirk_irram_cosech  :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_asinh   :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_acosh   :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_atanh   :: KirkFun11
foreign import ccall "kirk-irram-api.h" kirk_irram_asech   :: KirkFun11
--KIRK_API kirk_real_t * kirk_irram_acotanh(kirk_real_t *);
foreign import ccall "kirk-irram-api.h" kirk_irram_acosech :: KirkFun11
#endif
