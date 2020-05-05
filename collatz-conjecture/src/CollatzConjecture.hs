module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz number  | number == 1   = Just 1
                | even number   = collatz (div number 2)
                | otherwise     = collatz (3 * number + 1)

-- foldr 