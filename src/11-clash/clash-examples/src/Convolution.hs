{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Convolution where

import Clash.Prelude
import Data.Int
import Control.Lens (makeLenses, use, uses, (%=), (-=))
import Control.Monad.State.Strict
import Data.Word (Word8)
import GHC.Float (float2Int)
import Data.Tuple

type Pixel = Word8
type KernelWeight = Float

data CBState = 
   InitialFilling | Process
   deriving (Generic, NFDataX)

data CircularBufferState line_size kernel_size = CircularBufferState{
    _cbState :: CBState
    , _maskToRead :: Vec kernel_size Word8
    , _idToWrite  :: Word8
    , _lineSize   :: Integer
    , _pointer    :: Integer
  }
  deriving (Generic, NFDataX)

circularBufferRAM 
 :: forall kernel_size line_size dom. (KnownNat kernel_size, KnownNat line_size, KnownDomain dom, HiddenClockResetEnable dom)
 => Vec line_size Pixel
 -> Signal dom (Vec (kernel_size + 1) (Integer, Maybe (Integer, Pixel)))
 -> Signal dom (Vec (kernel_size + 1) Pixel) 
circularBufferRAM initialData inData =
    bundle $ zipWith (\f (x,y) -> f x y) ram (map unbundle (unbundle inData))
    where 
        lineBuffer = blockRam initialData
        ram = (repeat lineBuffer :: Vec (kernel_size + 1) _)
            --bundle $ map bundle $ map uncurry $ (repeat lineBuffer :: Vec (kernel_size + 1) _)

initialCircularBufferState
  :: forall line_size kernel_size dom . (KnownNat line_size, KnownNat kernel_size, KnownDomain dom, HiddenClockResetEnable dom) 
  => CircularBufferState line_size kernel_size
initialCircularBufferState = 
    CircularBufferState InitialFilling maskToRead 0 (natToInteger @line_size) 0
    where
        maskToRead = iterate (SNat :: SNat kernel_size) (+1) 1 :: Vec kernel_size Word8

circularBufferRead
  :: forall line_size kernel_size dom . (KnownNat line_size, KnownNat kernel_size, KnownDomain dom, HiddenClockResetEnable dom)  
  => Signal dom (CircularBufferState line_size kernel_size)
  -> Signal dom (Vec (kernel_size + 1) Pixel)
  -> Signal dom (Vec kernel_size Pixel)
circularBufferRead circularBufferState rawRamOut =
    mask <$> bundle (circularBufferState, rawRamOut)
    where
        mask (bufferState, rawOut) = 
            let maskToRead = _maskToRead bufferState in
            backpermute rawOut maskToRead

circularBufferWrite
  :: forall line_size kernel_size dom . (KnownNat line_size, KnownNat kernel_size, KnownDomain dom, HiddenClockResetEnable dom)  
  => (Signal dom (Vec (kernel_size + 1) (Integer, Maybe (Integer, Pixel))) -> Signal dom (Vec (kernel_size + 1) Pixel))
  -> Signal dom (CircularBufferState line_size kernel_size)
  -> Signal dom Pixel
  -> Signal dom (Vec (kernel_size + 1) Pixel)
circularBufferWrite circularBufferRam circularBufferState inData =
    circularBufferRam $ prepareInData <$> bundle (circularBufferState, inData)
    where
        prepareInData (bufferState, inPixel) =
            let idToWrite = _idToWrite bufferState in
            let currentAddress = _pointer bufferState in
            let dummyData = repeat (currentAddress, Nothing) :: Vec (kernel_size + 1) _ in
            replace idToWrite (0, Just (currentAddress, inPixel)) dummyData 

circularBuffer
 :: forall line_size kernel_size dom n . (KnownNat line_size, KnownNat kernel_size, KnownDomain dom, HiddenClockResetEnable dom, kernel_size ~ n + 1)  
  => CircularBufferState line_size kernel_size
  -> (Signal dom (Vec (kernel_size + 1) (Integer, Maybe (Integer, Pixel))) -> Signal dom (Vec (kernel_size + 1) Pixel))
  -> Signal dom Pixel
  -> Signal dom (Vec kernel_size Pixel)
circularBuffer circularBufferInitialState circularBufferRam inData =
    circularBufferRead circularBufferState $ circularBufferWrite circularBufferRam circularBufferState inData 
    where
        circularBufferState = register circularBufferInitialState (mealyF <$> circularBufferState)
        mealyF state = 
            newState
            where
                newState =
                    let curPointer = _pointer state in
                    let lineSize =  _lineSize state in
                    if curPointer == _lineSize state
                        then 
                            let curMaskToRead = _maskToRead state in
                            let curIdToWrite = _idToWrite state in
                            let newIdToWrite = head curMaskToRead in
                            let newMaskToRead = curMaskToRead <<+ curIdToWrite in
                            CircularBufferState InitialFilling newMaskToRead newIdToWrite lineSize 0
                        else 
                            CircularBufferState InitialFilling (_maskToRead state) (_idToWrite state) lineSize (curPointer + 1)


{-            
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
  => CircularBufferState line_size kernel_size
  -> (Signal dom (Vec (kernel_size + 1) (Integer, Maybe (Integer, Pixel))) -> Signal dom (Vec (kernel_size + 1) Pixel))
  -- (Vec (n + 1) (Vec (n + 1) v))
    -- dom Integer n1 (n + 1) Pixel
  -> Signal dom Pixel
  -> Signal dom (Vec kernel_size Pixel)
conv circularBufferInitialState circularBufferRam inputData =   
--conv kernel buff inputData = 
     --(foldr1 (+)) <$> (bundle $ zipWith convolution1D kernel (unbundle $ delayMaybe $ circularBuffer buff inputData))
     circularBuffer circularBufferInitialState circularBufferRam inputData     


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
    exposeClockResetEnable (conv (initialCircularBufferState :: CircularBufferState 640 3) 
                                 (circularBufferRAM initialData :: (Signal System (Vec 4 (Integer, Maybe (Integer, Pixel))) -> Signal System (Vec 4 Pixel))))-- :: (NFDataX  (CircularBuffer System Integer 10 3 Pixel)) => CircularBuffer System Integer 10 3 Pixel))
    where
        initialData = repeat 0 :: Vec 640 Pixel
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