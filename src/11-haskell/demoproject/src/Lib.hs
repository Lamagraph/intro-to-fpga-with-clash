module Lib
  ( someFunc, -- <1>
    addAndMult,
    fib,
  )
where

someFunc :: IO ()                       -- <2>
someFunc = putStrLn "My first print!"   -- <3>

addAndMult :: Num b => b -> b -> (b, b) -- <4>
addAndMult x y = (x + y, x * y)

fib :: Int -> Int -- <5>
fib 0 = 1         -- <6>
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2) -- <7>
