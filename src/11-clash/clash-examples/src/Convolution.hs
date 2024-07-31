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
type KernelWeight = Int

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

applyKernel 
  :: (KnownNat kernel_size, kernel_size ~ n + 1) 
  => Vec kernel_size KernelWeight 
  -> Vec kernel_size Pixel -> Pixel
applyKernel kernel inputData =
    -- fromIntegral $ float2Int $ foldr1 (+) $ zipWith (*) (map fromIntegral inputData) kernel
    fromIntegral $ foldr1 (+) $ zipWith (*) (map fromIntegral inputData) kernel

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

convolutionEngine
  :: (HiddenClockResetEnable dom, KnownNat line_size, KnownNat kernel_size, kernel_size ~ n + 1)
  => CircularBufferState line_size kernel_size
  -> (Signal dom (Vec (kernel_size + 1) (Integer, Maybe (Integer, Pixel))) -> Signal dom (Vec (kernel_size + 1) Pixel))
  -> (Vec kernel_size (Vec kernel_size KernelWeight))
  -> Signal dom Pixel
  -> Signal dom Pixel
convolutionEngine circularBufferInitialState circularBufferRam kernel inputData =   
   convolution2D kernel (unbundle $ circularBuffer circularBufferInitialState circularBufferRam inputData)

kernelSobel :: Vec 3 (Vec 3 KernelWeight)
kernelSobel =  
   (1 :> 0 :> -1 :> Nil) :>
   (2 :> 0 :> -2 :> Nil) :>
   (1 :> 0 :> -1 :> Nil) :>
   Nil

topEntity
  ::  Clock System
  -> Reset System
  -> Enable System
  -> Signal System Pixel
  -> Signal System Pixel
topEntity = 
    exposeClockResetEnable (convolutionEngine (initialCircularBufferState :: CircularBufferState 640 3) 
                                              (circularBufferRAM initialData :: (Signal System (Vec 4 (Integer, Maybe (Integer, Pixel))) -> Signal System (Vec 4 Pixel)))
                                              kernelSobel
    )
    where
        initialData = repeat 0 :: Vec 640 Pixel
