
import Data.Int
import Data.Number.Kirk
import Data.Number.Kirk.Debug
import qualified Data.Number.Kirk.Irram as Irram

putRealLn :: String -> KirkReal -> Int32 -> IO ()
putRealLn s x acc = do
  apx <- approx x (AbsAcc acc)
  putStrLn $ s ++ show apx
--  putStrLn $ "exponent = " ++ (show $ k_exponent $ radius apx)
--  putStrLn $ "mantissa = " ++ (show $ k_mantissa $ radius apx)
--  putStrLn $ "center = " ++ (show $ center apx)

main :: IO ()
main = do
  two      <- dyadicTestReal_d 2.0
  putRealLn "two: " two (-10)

  kr       <- dyadicTestReal_d 123.456

  Irram.init

  kr_sqrt  <- Irram.sqrt kr
  kr'      <- Irram.pow_n kr_sqrt 2
  kr''     <- Irram.pow   kr_sqrt two
  putRealLn "x           : " kr (-10)
  putRealLn "sqrt x      : " kr_sqrt (-10)
  putRealLn "(sqrt x)^2  : " kr' (-10)
  putRealLn "(sqrt x)^two: " kr'' (-10)

  irram_pi <- Irram.pi
  putRealLn "pi          : " irram_pi (-10)
