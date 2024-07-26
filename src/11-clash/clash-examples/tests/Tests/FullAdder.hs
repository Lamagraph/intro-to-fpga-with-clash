{-# OPTIONS_GHC -Wno-type-defaults #-}

module Tests.FullAdder where

import qualified Clash.Prelude as C
import FullAdder (fullAdder)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

prop_sumBit :: H.Property
prop_sumBit = H.property $ do
  a <- H.forAll Gen.bool -- <1>
  b <- H.forAll Gen.bool
  cIn <- H.forAll Gen.bool
  let aBit = C.boolToBit a -- <2>
      bBit = C.boolToBit b
      cInBit = C.boolToBit cIn
      actual = fullAdder aBit bBit cInBit
      sumBV = (C.boolToBV a + C.boolToBV b + C.boolToBV cIn) :: C.BitVector 2 -- <3>
      expected = (sumBV C.! 0, sumBV C.! 1)
  actual H.=== expected

accumTests :: TestTree
accumTests = $(testGroupGenerator) -- <4>

main :: IO ()
main = defaultMain accumTests
