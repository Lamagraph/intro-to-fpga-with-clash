{-# OPTIONS_GHC -Wno-type-defaults #-}

module Tests.Pairwise where

import qualified Clash.Prelude as C
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Pairwise (topEntity)
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

pairwise :: [b] -> [Maybe (b, b)]
pairwise l = case l of
  [] -> []
  (x : xs) -> case xs of
    (y : _) -> Just (x, y) : pairwise xs
    [] -> []

prop_sumReduceDefault :: H.Property
prop_sumReduceDefault = H.property $ do
  ls <- H.forAll $ Gen.list (Range.constant 0 10) $ Gen.int $ Range.constant 0 255
  let expected = pairwise ls
      actual = drop 1 $ C.simulateN (length ls) (topEntity C.clockGen C.resetGen C.enableGen) ls
  expected H.=== actual

accumTests :: TestTree
accumTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain accumTests