{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Convolution where

import Clash.Prelude
import Data.Int
import Control.Lens (makeLenses, use, uses, (%=), (-=))
import Control.Monad.State.Strict
import Data.Word (Word8)
import GHC.Float (float2Int)

type Pixel = Word8
type KernelWeight = Float

-- data LineBufferState = LBFilled | LBNotFilledYet

data LineBuffer dom n = LineBuffer{
     _ram :: Signal dom Integer -> Signal dom (Maybe (Integer, Pixel)) -> Signal dom Pixel
    , _size :: Integer
    , _firstFreeAddressToWrite :: Integer
  }
  deriving (Generic, NFDataX)

mkLineBuffer 
  :: forall line_size dom addr. (KnownNat line_size, KnownDomain dom, HiddenClockResetEnable dom)
  => LineBuffer dom line_size
mkLineBuffer =
    LineBuffer initial_value (natToInteger @line_size) 0
    where
        initial_value = blockRam (repeat 0 :: Vec line_size Pixel)

isFilled :: (LineBuffer _ _) -> Bool
isFilled buffer = (_size buffer) + 1 == _firstFreeAddressToWrite buffer

data CBState = 
   InitialFilling | Process
   deriving (Generic, NFDataX)

data CircularBuffer dom line_size kernel_size = CircularBuffer{
    _cbState :: CBState
    , _maskToRead :: Vec kernel_size Word8
    , _idToRead :: Word8
    , _buffers :: Vec (kernel_size + 1) (LineBuffer dom line_size)
  }
  deriving (Generic, NFDataX)

makeLenses ''CircularBuffer

initialCircularBuffer 
  :: forall line_size kernel_size dom . (KnownNat line_size, KnownNat kernel_size, KnownDomain dom, HiddenClockResetEnable dom) 
  => CircularBuffer dom line_size kernel_size  
initialCircularBuffer = 
    CircularBuffer InitialFilling maskToRead 0 initial_value
    where 
        lineBuffer = mkLineBuffer :: LineBuffer dom line_size 
        initial_value = repeat lineBuffer :: Vec (kernel_size + 1) (LineBuffer _ line_size)
        maskToRead = iterate (SNat :: SNat kernel_size) (+1) 1 :: Vec kernel_size Word8

circularBuffer
  :: forall line_size kernel_size dom . (KnownNat line_size, KnownNat kernel_size, KnownDomain dom, HiddenClockResetEnable dom)  
  => CircularBuffer dom line_size kernel_size
  -> Signal dom Pixel
  -> Signal dom (Vec kernel_size Pixel)
circularBuffer initialCircularBuffer inData =
    bufToWrite addrToRead ((\x -> Just (addrToWrite, x)) <$> inData)
    *> (bundle outData)
    where 
        lineBufToWrite = (_buffers initialCircularBuffer) !! (_idToRead initialCircularBuffer)
        bufToWrite = _ram lineBufToWrite
        addrToWrite = _firstFreeAddressToWrite lineBufToWrite
        addrToRead = pure 0 :: Signal dom Integer
        outData = map (\ram -> ram addrToRead (pure Nothing)) $ map (\lb -> _ram lb) $ backpermute (_buffers initialCircularBuffer) (_maskToRead initialCircularBuffer)        
    {-
    mealyS mealyF initialCircularBuffer inData
    where
        mealyF :: Pixel -> State  (CircularBuffer dom line_size kernel_size) (X kernel_size)--(Maybe (Vec kernel_size Pixel))  --LineBuffer dom addr line_size Pixel
        mealyF inD = do
            --btw <- use bufferToWrite
            s <- use cbState
            let nth = X Nothing :: (X kernel_size)--Maybe (Vec kernel_size Pixel)
            let z = X (Just (repeat 0)) :: (X kernel_size)  --Maybe (Vec kernel_size Pixel)
            case s of
              InitialFilling -> return nth
                    --if isFilled btw
                      --  then return nth
                        --else return nth
              Process ->  return nth --z
-}

applyKernel 
  :: (KnownNat kernel_size, kernel_size ~ n + 1) 
  => Vec kernel_size KernelWeight 
  -> Vec kernel_size Pixel -> Pixel
applyKernel kernel inputData =
    fromIntegral $ float2Int $ foldr1 (+) $ zipWith (*) (map fromIntegral inputData) kernel

mealyConvolution1D 
  :: (HiddenClockResetEnable dom, KnownNat kernel_size, kernel_size ~ n + 1) 
  => Vec kernel_size KernelWeight
  -> Signal dom (Vec kernel_size Pixel) 
  -> Signal dom Pixel
mealyConvolution1D kernel inputData =
    (applyKernel kernel) <$> inputData

convolution1D 
  :: (HiddenClockResetEnable dom, KnownDomain dom, KnownNat kernel_size, kernel_size ~ n + 1) 
  => Vec kernel_size KernelWeight 
  -> Signal dom Pixel 
  -> Signal dom Pixel
convolution1D kernel inputData =
    (mealyConvolution1D kernel) $ bundle windowed 
    where
        windowed = window inputData :: Vec _ (Signal _ _)

convolution2D 
  :: (HiddenClockResetEnable dom, KnownNat kernel_size, kernel_size ~ n + 1)
  => (Vec kernel_size (Vec kernel_size KernelWeight))
  -> Vec kernel_size (Signal dom Pixel)
  -> Signal dom Pixel
convolution2D kernel inputData =
    (foldr1 (+)) <$> (bundle $ zipWith convolution1D kernel inputData)

conv
  :: (HiddenClockResetEnable dom, KnownNat line_size, KnownNat kernel_size, kernel_size ~ n + 1)-- dom Integer n1 (1 + n) Pixel))
  => CircularBuffer dom line_size kernel_size   
  -- (Vec (n + 1) (Vec (n + 1) v))
    -- dom Integer n1 (n + 1) Pixel
   -> Signal dom Pixel
  -- -> Signal dom v
  -> Signal dom (Vec (n + 1) Pixel)
conv initialCircularBuffer inputData =   
--conv kernel buff inputData = 
     --(foldr1 (+)) <$> (bundle $ zipWith convolution1D kernel (unbundle $ delayMaybe $ circularBuffer buff inputData))
     circularBuffer initialCircularBuffer inputData     


kernelSobel :: Vec 3 (Vec 3 Int8)
kernelSobel =  
   (1 :> 0 :> -1 :> Nil) :>
   (2 :> 0 :> -2 :> Nil) :>
   (1 :> 0 :> -1 :> Nil) :>
   Nil

--initialBuf :: CircularBuffer System Integer 10 3 Pixel
--initialBuf = mkCircularBuffer 10 3

topEntity
  ::  Clock System
  -> Reset System
  -> Enable System
  -- -> (Vec 3 (Vec 3 Int8)) 
  -- -> Vec 3 (Signal System Int8)
  -> Signal System Pixel
  -- -> Signal System Int8
  -> Signal System (Vec 3 Pixel)
--topEntity = exposeClockResetEnable (convolution2D  kernelSobel)
topEntity = 
    exposeClockResetEnable (conv (initialCircularBuffer :: CircularBuffer System 640 3))-- :: (NFDataX  (CircularBuffer System Integer 10 3 Pixel)) => CircularBuffer System Integer 10 3 Pixel))
    --where
        --initialBuf = initialCircularBuffer :: CircularBuffer System Integer 10 3 Pixel

    

{-
data DelayState = DelayState
  { _history    :: Vec 4 Int
  , _untilValid :: Index 4
  }
  deriving (Generic, NFDataX)
makeLenses ''DelayState

initialDelayState = DelayState (repeat 0) maxBound

--delayS :: Int -> State DelayState (Maybe Int)
delayS n = do
  history   %= (n +>>)
  remaining <- use untilValid
  if remaining > 0
  then do
     untilValid -= 1
     return Nothing
   else do
     out <- uses history last
     return (Just out)

topEntity ::Clock System
  -> Reset System
  -> Enable System -> Signal System Int -> Signal System (Maybe Int)
topEntity = exposeClockResetEnable (mealyS delayS initialDelayState)
-}