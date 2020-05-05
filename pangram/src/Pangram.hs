module Pangram (isPangram) where
import Data.List
import Data.Char


-- isLetter c = 'a' <= c && c <= 'z'

isPangram :: String -> Bool
isPangram text  | length (group $ sort [toLower x | x <- text, isAscii x, isAlpha x]) == 26 = True
                | otherwise                                                                 = False

-- main = print (isPangram "The quick brown fox jumps over the lazy dog.")