module SumReduce where

import Clash.Prelude

sumReduce
  :: Int
  -> Int
  -> (Int, Int)
sumReduce acc num = (mySum, mySum)
  where
    mySum = acc + num

mealySumReduce
  :: (KnownDomain dom, HiddenClockResetEnable dom)
  => Signal dom Int
  -> Signal dom Int
mealySumReduce = mealy sumReduce 0

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "sumReduce"
      , t_inputs = [PortName "CLK", PortName "RST", PortName "ENBL", PortName "NUM"]
      , t_output = PortName "SUM"
      }
  )
  #-}
topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Int
  -> Signal System Int
topEntity = exposeClockResetEnable mealySumReduce

-- вот так в симуляторе можно глазами проверить всё
-- L.take 5 $ simulate @System (topEntity clockGen resetGen enableGen) [1, 2, 3, 4, 5]
