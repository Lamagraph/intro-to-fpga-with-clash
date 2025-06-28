module FullAdder (fullAdder) where

import Clash.Annotations.TH -- <1>
import Clash.Prelude -- <2>

fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit) -- <3>
fullAdder a b c_in = (res_sum, c_out)
  where
    res_sum = a `xor` b `xor` c_in
    c_out = (a .&. b) .|. (c_in .&. (a `xor` b))

topEntity -- <4>
  :: "a" ::: Bit -- <5>
  -> "b" ::: Bit
  -> "c_in" ::: Bit
  -> ("sum" ::: Bit, "c_out" ::: Bit)
topEntity = fullAdder

makeTopEntity 'topEntity -- <6>
