
import Data.Number.Kirk --(KirkRealT, KirkReal, KirkSeq0Idx(AbsAcc))
import qualified Data.Number.Kirk.Irram as Irram

foreign import ccall "kirk_test_real" kirk_test_real :: Double -> IO KirkPtr

main :: IO ()
main = do
  Irram.init
  kr   <- kirk_test_real 123.456 >>= kirk10
  let kf_sqrt = Irram.sqrt
  --fr  <- get_real_ptr r
  --apx <- withForeignPtr fr $ \p -> do_real_apx_abs p $ AbsAcc (-10)
  --apx <- approx (KirkReal fr) $ AbsAcc (-10)
  kr_sqrt <- kf_sqrt kr
  apx <- approx kr $ AbsAcc (-10)
  apx_sqrt <- approx kr_sqrt $ AbsAcc (-10)
  putStrLn $ "exponent = " ++ (show $ k_exponent $ radius apx)
  putStrLn $ "mantissa = " ++ (show $ k_mantissa $ radius apx)
  putStrLn $ "center = " ++ (show $ center apx)
