{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Number.Kirk.AERN2 where

-------------------------------------------------------------------
-- AERN2 support code

import qualified MixedTypesNumPrelude as MTN
import System.IO.Unsafe
import Data.Convertible
import AERN2.MP.UseMPFR.Float
import AERN2.MP.Dyadic
import AERN2.Real

import Data.Number.Kirk (KirkImportReal(..),KirkSeq0Idx(..),KirkReal,Approximation(..),KirkBoundT(..))

instance KirkImportReal CauchyReal where
  approx r (Effort eff) = approx r (AbsAcc (convert eff))
  approx r (AbsAcc acc) = 
    return $ Approximation rad c
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
      (Approximation rad c) = unsafePerformIO (approx r (AbsAcc acc))
      (KirkBoundT e m) = rad
      radF 
          | e >= 0 = encodeFloat (convert m) (convert e) :: MPFloat
          | otherwise = encodeFloat (convert m * 2^(-e)) 0

-- end of AERN2 support code
-------------------------------------------------------------------

