-- module CollatzConjecture (collatz) where
-- import Data.List(genericLength)

-- collatz :: Integer -> Maybe Integer
-- collatz n   | n > 0     = Just (genericLength (collatzSeq n) - 1)
--             | otherwise = Nothing
--                 where   collatzSeq 0    = [0]
--                         collatzSeq 1    = [1]
--                         collatzSeq x    | even x    = x : collatzSeq (div x 2)
--                                         | otherwise = x : collatzSeq (3 * x + 1)

module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
    | n <= 0 = Nothing
    | n == 1 = Just 0
    | mod n 2 == 0 = fmap (+1) $ collatz (n `div` 2)
    | otherwise = fmap (+1) $ collatz (n*3+1)