module Pangram (isPangram) where
import Data.Set (Set)
import Data.Char
import qualified Data.Set as Set
-- need to add containers as dependency to package.yml
isPangram :: String -> Bool
isPangram text = Set.fromList [toLower a | a <- text, isLetter a  ] == Set.fromList ['a'..'z']

