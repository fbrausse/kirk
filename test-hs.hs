{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Int
import Data.Number.Kirk
import Data.Number.Kirk.Debug
import qualified Data.Number.Kirk.Irram as Irram

-------------------------------------------------------------------
-- AERN2 support code

import qualified MixedTypesNumPrelude as MTN
import System.IO.Unsafe
import Data.Convertible
import AERN2.MP.Float
import AERN2.MP.Dyadic
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

toAERN2CR :: KirkReal -> CauchyReal
toAERN2CR r =
  newCR "kirk" [] makeQ
    where
    makeQ (_me, _src) (AccuracySG acS _acG) =
      updateRadius (MTN.+ (errorBound radF)) (mpBall (dyadic c))
      where
      acc = convert (- (fromAccuracy acS))
      (KirkApxT rad c) = unsafePerformIO (approx r (AbsAcc acc))
      (KirkBoundT e m) = rad
      radF 
          | e >= 0 = encodeFloat (convert m) (convert e) :: MPFloat
          | otherwise = encodeFloat (convert m * 2^(-e)) 0

-- end of AERN2 support code
-------------------------------------------------------------------

putRealLn :: (KirkImportReal r) => String -> r -> Int32 -> IO ()
putRealLn s x acc = do
  apx <- approx x (AbsAcc acc)
  putStrLn $ s ++ show apx
--  putStrLn $ "exponent = " ++ (show $ k_exponent $ radius apx)
--  putStrLn $ "mantissa = " ++ (show $ k_mantissa $ radius apx)
--  putStrLn $ "center = " ++ (show $ center apx)

main :: IO ()
main = do
  main2

-- using AERN2 CauchyReal
main2 :: IO ()
main2 = do
  let twoCR = real (2::Integer)
  putRealLn "two         : " twoCR (-10)
  let thirdCR = real (1/3::Rational)
  putRealLn "third       : " thirdCR (-10)
  
  let krCR = real (123.456::Rational)
  
  Irram.init

  kr_sqrt  <- Irram.sqrt krCR
  let kr_sqrtCR = toAERN2CR kr_sqrt

  kr'      <- Irram.pow_n kr_sqrt 2
  kr''     <- Irram.pow   kr_sqrtCR twoCR
-- the above line leads to errors such as:
{-
test-hs: DataziNumberziKirk_d2r2: interrupted
internal error: RELEASE_LOCK: I do not own this lock: rts/sm/Storage.c 1334
    (GHC version 7.10.3 for x86_64_unknown_linux)
    Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
Aborted (core dumped)
-}
  
  putRealLn "x           : " krCR (-10)
  putRealLn "sqrt x      : " kr_sqrtCR (-10)
  putRealLn "(sqrt x)^2  : " kr' (-10)
--   putRealLn "(sqrt x)^two: " kr'' (-10)
-- the above line leads to the following error:
{-
*** Error in `./test-hs': realloc(): invalid pointer: 0x00007f3fe644b108 ***
Aborted (core dumped)
-}

  irram_pi <- Irram.pi
  let irram_piCR = toAERN2CR irram_pi
  
  putRealLn "pi          : " irram_piCR (-10)
  
  return ()

-- using KirkReal
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
