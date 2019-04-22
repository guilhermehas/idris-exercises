over_length : Nat -> List String -> Nat
over_length k xs = length $ filter (> k) $ map length xs
