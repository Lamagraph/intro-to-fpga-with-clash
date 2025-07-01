module Pairwise where

import Clash.Prelude

pairwiseAcc
  :: Maybe Int
  -> Int
  -> (Maybe Int, Maybe (Int, Int))
pairwiseAcc state inputInt = case state of
  Just s -> (Just inputInt, Just (s, inputInt))
  Nothing -> (Just inputInt, Nothing)

mealyPairwise
  :: (KnownDomain dom, HiddenClockResetEnable dom)
  => Signal dom Int
  -> Signal dom (Maybe (Int, Int))
mealyPairwise = mealy pairwiseAcc Nothing

topEntity -- <1>
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Int
  -> Signal System (Maybe (Int, Int))
topEntity = exposeClockResetEnable mealyPairwise
