{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Convolution where

import Clash.Prelude
import Data.Word

mapSignal :: HiddenClockResetEnable dom => (a -> b) -> Signal dom a -> Signal dom b
mapSignal f signal = 
    mealyI (\s d -> (s, f d)) 0 signal
    where 
        mealyI = mealy :: HiddenClockResetEnable dom => (Int -> a -> (Int, b)) -> Int -> Signal dom a -> Signal dom b

applyKernel :: (KnownNat (n + 1), Num v) => Vec (n + 1) v -> Vec (n + 1) v -> v
applyKernel kernel inputData =
    foldr1 (+) $ zipWith (*) inputData kernel

mealyConvolution1D :: (HiddenClockResetEnable dom, KnownNat (n + 1), Num v) => (Vec (n + 1) v) -> Signal dom (Vec (n + 1) v) -> Signal dom v
mealyConvolution1D kernel inputData =
    mapSignal (applyKernel kernel) inputData

convolution1D :: (HiddenClockResetEnable dom, KnownDomain dom, KnownNat (n + 1), Num v, Default v, NFDataX v) => Vec (n + 1) v -> Signal dom v -> Signal dom v
convolution1D kernel inputData =
    (mealyConvolution1D kernel) $ bundle windowed 
    where
        windowed = window inputData :: Vec _ (Signal _ _)

convolution2D :: (HiddenClockResetEnable dom, KnownNat (n + 1), Num v, Default v, NFDataX v) => (Vec (n + 1) (Vec (n + 1) v)) -> Vec (n + 1) (Signal dom v) -> Signal dom v
convolution2D kernel inputData =
    mapSignal (foldr1 (+)) $ bundle $ zipWith convolution1D kernel inputData

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> (Vec 3 (Vec 3 Word8)) 
  -> Vec 3 (Signal System Word8)
  -> Signal System Word8
topEntity = exposeClockResetEnable convolution2D 

-- kernel :: Vec 3 (Vec 3 Int8)
-- kernel = ( 
--    (1 :> 0 :> -1 :> Nil) :>
--    (2 :> 0 :> -2 :> Nil) :>
--    (1 :> 0 :> -1 :> Nil) :>
--    Nil
--    )