{-# OPTIONS_GHC -Wno-type-defaults #-}

module Tests.SumReduce where

import qualified Clash.Prelude as C
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import SumReduce (topEntity)
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

prop_sumReduceDefault :: H.Property
prop_sumReduceDefault = H.property $ do
  ls <- H.forAll $ Gen.list (Range.constant 0 10) $ Gen.int $ Range.constant 0 255 -- <1>
  let expected = scanl1 (+) ls -- <2>
      actual = C.simulateN (length ls) (topEntity C.clockGen C.resetGen C.enableGen) ls -- <3>
  expected H.=== actual

accumTests :: TestTree
accumTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain accumTests
