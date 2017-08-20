{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Int
import Data.Number.Kirk
import Data.Number.Kirk.Debug
import qualified Data.Number.Kirk.Irram as Irram

import Data.Convertible
import AERN2.MP.Float
-- import AERN2.MP.Dyadic
import AERN2.Real

instance KirkImportReal CauchyReal where
  approx r (Effort eff) = approx r (AbsAcc (convert eff))
  approx r (AbsAcc acc) = 
    return $ KirkApxT rad c
    where
    b = r ? (bitsS $ negate $ toInteger acc)
    c = mpFloat $ AERN2.Real.centre b
    radF = mpFloat $ AERN2.Real.radius b
    (m,e) = decodeFloat radF
    rad = KirkBoundT (convert e) (convert m)

fromAERN2CR :: CauchyReal -> IO KirkReal
fromAERN2CR r = makeHsReal r >>= kirk10
    
putRealLn :: String -> KirkReal -> Int32 -> IO ()
putRealLn s x acc = do
  apx <- approx x (AbsAcc acc)
  putStrLn $ s ++ show apx
--  putStrLn $ "exponent = " ++ (show $ k_exponent $ radius apx)
--  putStrLn $ "mantissa = " ++ (show $ k_mantissa $ radius apx)
--  putStrLn $ "center = " ++ (show $ center apx)

main :: IO ()
main = do
  main2

main2 :: IO ()
main2 = do
  third <- fromAERN2CR (real (1/3::Rational))
  putRealLn "third: " third (-10)
  
  kr       <- fromAERN2CR (real (123.456::Rational))
  
  Irram.init

  kr_sqrt  <- Irram.sqrt kr
  putRealLn "x           : " kr (-10)
  putRealLn "sqrt x      : " kr_sqrt (-10)
  
  return ()

main1 :: IO ()
main1 = do
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
