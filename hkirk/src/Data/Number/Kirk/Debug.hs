
module Data.Number.Kirk.Debug (
  dyadicTestReal_d
) where

import Data.Number.Kirk

import Foreign.C.Types (CDouble(..))

--foreign import ccall "kirk-dyadic-real.h" kirk_test_real :: IO KirkRealPtr

foreign import ccall "kirk-dyadic-real.h" kirk_dyadic_test_real_d :: CDouble -> IO KirkRealPtr

-- can't provide interface with Double due to realToFrac issues:
-- <https://ghc.haskell.org/trac/ghc/ticket/3676>
dyadicTestReal_d :: CDouble -> IO KirkReal
dyadicTestReal_d x = kirk_dyadic_test_real_d x >>= kirk10
