module Lib
  ( someFunc, -- <2>
    addAndMult,
    fib,
  )
where

someFunc :: IO () -- <3>
someFunc = putStrLn "My first print!" -- <1>

addAndMult :: Num b => b -> b -> (b, b) -- <4>
addAndMult x y = (x + y, x * y)

fib :: Int -> Int -- <5>
fib 0 = 1 -- <7>
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2) -- <6>
