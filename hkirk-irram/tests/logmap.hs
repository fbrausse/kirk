
{-# LANGUAGE ForeignFunctionInterface #-}

import Data.Number.Kirk
import qualified Data.Number.Kirk.Irram as Irram
import Data.Number.Kirk.Debug
import Data.Word

foreign import ccall "" kirk_irram_logmap
  :: Word32 -> KirkRealPtr -> KirkRealPtr -> IO KirkRealPtr

logmap :: (KirkImportReal a, KirkImportReal b)
       => Word32 -> a -> b -> IO KirkReal
logmap n = kirk12 $ kirk_irram_logmap n

main :: IO ()
main = do
  Irram.init
  let n = 1000
  let p = -300 -- set to -400 to see a crash: hmpfr's limbs can't be realloc'ed
               -- by std mpfr-functions
  c <- dyadicTestReal_d 3.75
  x0 <- dyadicTestReal_d 0.5
  xn <- logmap n c x0
  an <- approx xn $ AbsAcc p
  putStrLn $ show an
