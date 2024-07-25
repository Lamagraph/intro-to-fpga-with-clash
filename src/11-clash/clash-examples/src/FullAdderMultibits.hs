{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module FullAdderMultibits where

import Clash.Annotations.TH
import Clash.Prelude
import FullAdder (fullAdder)

fullAdderMultiBits
  :: (KnownNat n) -- <1>
  => Vec n Bit -- <2>
  -> Vec n Bit
  -> Bit
  -> (Vec n Bit, Bit)
fullAdderMultiBits a b c_in = res
  where
    zero = repeat 0 :: Vec _ Bit -- <3>
    res = foldr func (zero, c_in) (zip a b) -- <4>
    func (fstBit, sndBit) (ansVec, prevCarry) = (resBit +>> ansVec, nextCarry) -- <5>
      where
        (resBit, nextCarry) = fullAdder fstBit sndBit prevCarry

topEntity
  :: "a" ::: Vec 8 Bit
  -> "b" ::: Vec 8 Bit
  -> "c_in" ::: Bit
  -> ( "sum" ::: Vec 8 Bit
     , "c_out" ::: Bit
     )
topEntity = fullAdderMultiBits

makeTopEntity 'topEntity
