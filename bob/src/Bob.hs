module Bob (responseFor) where
import Data.Char (isSpace, isNumber, isAlpha, isAlphaNum, isUpper)


responseFor :: String -> String
responseFor xs  | all isSpace xs                                            = "Fine. Be that way!"
                | last xs == '?' && allIsUpper xs && not (allIsAlphaNum xs) = "Calm down, I know what I'm doing!"
                | allIsUpper xs && not (allIsAlphaNum xs)                   = "Whoa, chill out!"
                | last (trim xs) == '?'                                     = "Sure."
                | otherwise                                                 = "Whatever."

allIsUpper ls = all isUpper (filter isAlpha ls)
allIsAlphaNum ns = all isNumber (filter isAlphaNum ns)
trim = unwords . words
