module DNABestSolution (toRNA) where

dnaComplement :: Char -> Either Char Char
dnaComplement 'G' = Right 'C'
dnaComplement 'C' = Right 'G'
dnaComplement 'T' = Right 'A'
dnaComplement 'A' = Right 'U'
dnaComplement  x  = Left x

toRNA :: String -> Either Char String
toRNA xs = mapM dnaComplement xs