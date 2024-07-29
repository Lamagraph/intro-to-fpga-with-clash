{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Convolution where

import Clash.Prelude
import Data.Int
import Control.Lens (makeLenses, use)

type Pixel = Unsigned 8

data LineBufferState = LBFilled | LBNotFilledYet

data LineBuffer dom addr n a = LineBuffer{
    --_ram :: Signal dom addr -> Signal dom (Maybe (addr, a)) -> Signal dom a
     _size :: Integer
    , _firstFreeAddressToWrite :: addr
  }
  deriving (Generic, NFDataX)

mkLineBuffer 
  :: forall n dom addr. (KnownNat n, KnownDomain dom, HiddenClockResetEnable dom, Enum addr, NFDataX addr, Num addr)
  => LineBuffer dom addr n Pixel
mkLineBuffer =
    --LineBuffer initial_value (natToInteger @n) 0
    LineBuffer (natToInteger @n) 0
    --where
      --  initial_value = blockRam (repeat 0 :: Vec n Pixel)

isFilled :: (LineBuffer _ _ _ _) -> Bool
isFilled buffer = (_size buffer) + 1 == _firstFreeAddressToWrite buffer

data State = InitialFilling | Process

--data CircularBuffer dom addr n1 n2 a = CircularBuffer{
data CircularBuffer = CircularBuffer{
    _state :: Integer -- :: State
    , _bufferToWrite :: Integer -- :: LineBuffer dom addr n1 a
    , _buffersToRead :: Integer -- :: Vec n2 (LineBuffer dom addr n1 a)
  }
  deriving (Generic, NFDataX)

makeLenses ''CircularBuffer

mkCircularBuffer 
  :: -- forall n1 n2 dom addr . (KnownNat n1, KnownNat n2, KnownDomain dom, HiddenClockResetEnable dom, Enum addr, NFDataX addr,Num addr) 
  -- => CircularBuffer -- dom addr n1 n2 Pixel
  CircularBuffer
mkCircularBuffer =
    buffer
    where 
        --lineBuffer = mkLineBuffer :: LineBuffer dom addr n1 Pixel 
        --initial_value = repeat lineBuffer :: Vec n2 (LineBuffer _ _ _ _)
        --buffer = CircularBuffer 1 mkLineBuffer initial_value
        buffer = CircularBuffer 1 0 0

circularBuffer 
  :: (KnownNat n2, KnownDomain dom, HiddenClockResetEnable dom, NFDataX (CircularBuffer))-- dom Integer n1 n2 a1)) 
  => CircularBuffer -- dom Integer n1 n2 a1
  -> Signal dom i 
  -> Signal dom (Maybe (Vec n2 a2))
circularBuffer buffer inData =
    mealyS mealyF buffer inData
    where         
        mealyF inD = do
            btw <- use bufferToWrite
            s <- use state
            let nth = Nothing -- repeat Nothing :: Vec _ _
            case s of
                 1 -> 
                     if btw > 0 -- isFilled bufferToWrite
                         then return nth
                         else return nth
                 2 ->  return nth

applyKernel :: (KnownNat (n + 1), Num v) => Vec (n + 1) v -> Vec (n + 1) v -> v
applyKernel kernel inputData =
    foldr1 (+) $ zipWith (*) inputData kernel

mealyConvolution1D 
  :: (HiddenClockResetEnable dom, KnownNat (n + 1), Num v) 
  => (Vec (n + 1) v) 
  -> Signal dom (Vec (n + 1) v) 
  -> Signal dom v
mealyConvolution1D kernel inputData =
    (applyKernel kernel) <$> inputData

convolution1D 
  :: (HiddenClockResetEnable dom, KnownDomain dom, KnownNat (n + 1), Num v, Default v, NFDataX v) 
  => Vec (n + 1) v 
  -> Signal dom v 
  -> Signal dom v
convolution1D kernel inputData =
    (mealyConvolution1D kernel) $ bundle windowed 
    where
        windowed = window inputData :: Vec _ (Signal _ _)

convolution2D 
  :: (HiddenClockResetEnable dom, KnownNat (n + 1), Num v, Default v, NFDataX v)
  => (Vec (n + 1) (Vec (n + 1) v))
  -> Vec (n + 1) (Signal dom v)
  -> Signal dom v
convolution2D kernel inputData =
    (foldr1 (+)) <$> (bundle $ zipWith convolution1D kernel inputData)

conv
  :: (HiddenClockResetEnable dom, KnownNat (n + 1), Num v, Default v, NFDataX v, NFDataX  (CircularBuffer))-- dom Integer n1 (1 + n) Pixel))
  => -- (Vec (n + 1) (Vec (n + 1) v))
   CircularBuffer -- dom Integer n1 (n + 1) Pixel
  -> Signal dom v
  -- -> Signal dom v
  -> Signal dom (Maybe (Vec (n + 1) a2))
conv buff inputData =   
--conv kernel buff inputData = 
     --(foldr1 (+)) <$> (bundle $ zipWith convolution1D kernel (unbundle $ delayMaybe $ circularBuffer buff inputData))
     circularBuffer buff inputData     


kernelSobel :: Vec 3 (Vec 3 Int8)
kernelSobel =  
   (1 :> 0 :> -1 :> Nil) :>
   (2 :> 0 :> -2 :> Nil) :>
   (1 :> 0 :> -1 :> Nil) :>
   Nil

--initialBuf :: CircularBuffer System Integer 10 3 Pixel
--initialBuf = mkCircularBuffer 10 3

topEntity
  :: (NFDataX  (CircularBuffer)) -- System Integer 10 3 Pixel)) 
  => Clock System
  -> Reset System
  -> Enable System
  -- -> (Vec 3 (Vec 3 Int8)) 
  -- -> Vec 3 (Signal System Int8)
  -> Signal System Int8
  -- -> Signal System Int8
  -> Signal System (Maybe (Vec 3 Int8))
--topEntity = exposeClockResetEnable (convolution2D  kernelSobel)
topEntity = 
    exposeClockResetEnable (conv (mkCircularBuffer ))-- :: (NFDataX  (CircularBuffer System Integer 10 3 Pixel)) => CircularBuffer System Integer 10 3 Pixel))
  --  where
    --    initialBuf = mkCircularBuffer :: CircularBuffer System Integer 10 3 Pixel