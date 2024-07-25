{-# LANGUAGE TemplateHaskell #-} -- <1>

import           Hedgehog
import qualified Hedgehog.Gen    as Gen
import qualified Hedgehog.Range  as Range -- <2>
import           Lib                      -- <3>

prop_fibIsPositive :: Property -- <4>
prop_fibIsPositive =
  property $ do 
    x <- forAll $ Gen.int (Range.constant 0 30)  -- <5>
    assert $ fib x > 0                           -- <6>

prop_fibIsFib :: Property
prop_fibIsFib =
  property $ do
    x <- forAll $ Gen.int (Range.constant 2 30)
    fib x === fib (x - 2) + fib (x - 1) -- <7>

prop_fibIsMonotonic :: Property
prop_fibIsMonotonic =
  property $ do
    x <- forAll $ Gen.int (Range.constant 1 30)
    assert $ fib (x + 1) > fib x

main :: IO Bool               -- <8>
main =
  checkParallel $$(discover)  -- <9>
