
{-# LANGUAGE ForeignFunctionInterface #-}

import Data.Number.Kirk
import Foreign.Ptr
import Foreign.ForeignPtr

foreign import ccall "kirk_test_real" kirk_test_real :: Double -> IO (Ptr KirkRealT)

main :: IO ()
main = do
  fr <- get_real_ptr $ kirk_test_real 123.456
  --apx <- withForeignPtr fr $ \p -> do_real_apx_abs p $ AbsAcc (-10)
  apx <- approx (KirkReal fr) $ AbsAcc (-10)
  putStrLn $ "exponent = " ++ (show $ k_exponent $ radius apx)
  putStrLn $ "mantissa = " ++ (show $ k_mantissa $ radius apx)
  putStrLn $ "center = " ++ (show $ center apx)
