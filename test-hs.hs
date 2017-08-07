
{-# LANGUAGE ForeignFunctionInterface #-}

import Data.Number.Kirk
import Foreign.Ptr
import Foreign.ForeignPtr

foreign import ccall "get_test_real" get_test_real :: IO (Ptr KirkRealT)

main :: IO ()
main = do
  fr <- get_real_ptr get_test_real
  apx <- withForeignPtr fr $ \p -> do_real_apx_abs p 20
  let r = radius apx
  putStrLn $ "exponent = " ++ (show $ k_exponent r)
  putStrLn $ "center = " ++ (show $ center apx)

