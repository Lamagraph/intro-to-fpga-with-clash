module FullAdderNum where

import Clash.Annotations.TH
import Clash.Prelude

-- Тут как и в обычном SystemVerilog, тоже знаковые 64-битные Int-ы
topEntity
  :: "a" ::: Int
  -> "b" ::: Int
  -> "sum" ::: Int
topEntity a b = a + b

makeTopEntity 'topEntity
