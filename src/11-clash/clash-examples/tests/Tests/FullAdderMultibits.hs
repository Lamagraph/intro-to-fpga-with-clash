module Tests.FullAdderMultibits where

import Clash.Prelude as C
import FullAdderMultibits (fullAdderMultiBits)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

prop_sumRes :: H.Property
prop_sumRes = H.property $ do
  a <- H.forAll $ Gen.int (Range.constant 0 255)
  b <- H.forAll $ Gen.int (Range.constant 0 255)
  cIn <- H.forAll Gen.bool
  let aUns = toEnum a :: C.Unsigned 8
      bUns = toEnum b :: C.Unsigned 8
      cInBV = C.boolToBV cIn :: C.BitVector 8
      expectedBV = (C.pack aUns + C.pack bUns + cInBV) :: C.BitVector 8
      expected = C.bv2v expectedBV
      actual = fst $ fullAdderMultiBits (C.bitCoerce aUns) (C.bitCoerce bUns) (boolToBit cIn)
  expected H.=== actual

accumTests :: TestTree
accumTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain accumTests
