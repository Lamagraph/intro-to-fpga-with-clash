{-# LANGUAGE PartialTypeSignatures #-}

module Pairwise where

import Clash.Prelude

-- Вообще, у clash есть уже windows1d (https://hackage.haskell.org/package/clash-prelude-1.8.1/docs/Clash-Explicit-Prelude.html#v:windows1d)
-- Но можно попробовать подумать и реализовать через автоматы
-- У Clash видимо какая-то очень хорошая поддержка Maybe, так что оно вот в таком виде синтезируется без танцев с бубнами

automaton
  :: Maybe Int
  -> Int
  -> (Maybe Int, Maybe (Int, Int))
automaton state inputInt = case state of
  Just s -> (Just inputInt, Just (s, inputInt))
  Nothing -> (Just inputInt, Nothing)

mealyAutomaton
  :: (KnownDomain dom, HiddenClockResetEnable dom)
  => Signal dom Int
  -> Signal dom (Maybe (Int, Int))
mealyAutomaton = mealy automaton Nothing

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Int
  -> Signal System (Maybe (Int, Int))
topEntity = exposeClockResetEnable mealyAutomaton
