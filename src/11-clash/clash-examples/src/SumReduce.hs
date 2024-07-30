module SumReduce where

import Clash.Prelude

sumReduceAcc -- <1>
  :: Int
  -> Int
  -> (Int, Int)
sumReduceAcc acc num = (mySum, mySum)
  where
    mySum = acc + num

mealySumReduce
  :: (KnownDomain dom, HiddenClockResetEnable dom) -- <2>
  => Signal dom Int -- <2>
  -> Signal dom Int
mealySumReduce = mealy sumReduceAcc 0

{-# ANN
  topEntity -- <3>
  ( Synthesize
      { t_name = "sumReduce"
      , t_inputs = [PortName "CLK", PortName "RST", PortName "ENBL", PortName "NUM"]
      , t_output = PortName "SUM"
      }
  )
  #-}
topEntity
  :: Clock System -- <4>
  -> Reset System
  -> Enable System
  -> Signal System Int -- <5>
  -> Signal System Int
topEntity = exposeClockResetEnable mealySumReduce -- <6>
