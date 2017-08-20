
import Data.Int
import Data.Number.Kirk
import Data.Number.Kirk.Debug
import qualified Data.Number.Kirk.Irram as Irram

putRealLn :: KirkReal -> Int32 -> IO ()
putRealLn x acc = do
  apx <- approx x (AbsAcc acc)
  putStrLn $ show apx
--  putStrLn $ "exponent = " ++ (show $ k_exponent $ radius apx)
--  putStrLn $ "mantissa = " ++ (show $ k_mantissa $ radius apx)
--  putStrLn $ "center = " ++ (show $ center apx)

main :: IO ()
main = do
  two      <- dyadicTestReal_d 2.0
  putRealLn two (-10)

  kr       <- dyadicTestReal_d 123.456

  Irram.init

  kr_sqrt  <- Irram.sqrt kr
  kr'      <- Irram.pow_n kr_sqrt 2
  kr''     <- Irram.pow   kr_sqrt two
  putRealLn kr (-10)
  putRealLn kr_sqrt (-10)
  putRealLn kr' (-10)
  putRealLn kr'' (-10)

  irram_pi <- Irram.pi
  putRealLn irram_pi (-10)
