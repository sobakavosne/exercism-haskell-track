module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs    | null xs       = Right ""
            | xs == "G"     = Right "C"
            | xs == "C"     = Right "G"
            | xs == "T"     = Right "A"
            | xs == "A"     = Right "U"
            | otherwise     = traverse charToRNA xs
                where charToRNA x   | x == 'G'  = Right 'C'
                                    | x == 'C'  = Right 'G'
                                    | x == 'T'  = Right 'A'
                                    | x == 'A'  = Right 'U'
                                    | otherwise = Left x
