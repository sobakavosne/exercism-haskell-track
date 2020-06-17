module BobBestSolution (responseFor) where

import Data.Char (isSpace, isLower, isLetter)
import Data.List (isSuffixOf)

responseFor :: String -> String
responseFor message
  | isQuiet = "Fine. Be that way!"
  | isShouting = "Whoa, chill out!"
  | isAsking = "Sure."
  | otherwise = "Whatever."
  where
    isQuiet = all isSpace message
    isShouting = not (any isLower message) && any isLetter message 
    isAsking = "?" `isSuffixOf` message