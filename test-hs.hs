
import Data.Number.Kirk --(KirkRealT, KirkReal, KirkSeq0Idx(AbsAcc))
import Data.Number.Kirk.Irram --(KirkRealT, KirkReal, KirkSeq0Idx(AbsAcc))
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Utils

foreign import ccall "kirk_test_real" kirk_test_real :: Double -> IO (Ptr KirkRealT)

foreign import ccall "iRRAM_initialize" iRRAM_initialize :: CInt -> Ptr CString -> IO ()

main :: IO ()
main = do
  withCString "test-hs" $ \argv0 -> with argv0 $ iRRAM_initialize 0
  kr   <- kirk_test_real 123.456 >>= kirk10
  let kf_sqrt = kirk11 kirk_irram_sqrt :: KirkReal -> IO KirkReal
  --fr  <- get_real_ptr r
  --apx <- withForeignPtr fr $ \p -> do_real_apx_abs p $ AbsAcc (-10)
  --apx <- approx (KirkReal fr) $ AbsAcc (-10)
  kr_sqrt <- kf_sqrt kr
  apx <- approx kr $ AbsAcc (-10)
  apx_sqrt <- approx kr_sqrt $ AbsAcc (-10)
  putStrLn $ "exponent = " ++ (show $ k_exponent $ radius apx)
  putStrLn $ "mantissa = " ++ (show $ k_mantissa $ radius apx)
  putStrLn $ "center = " ++ (show $ center apx)
