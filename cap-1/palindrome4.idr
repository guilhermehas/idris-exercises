palindrome : Nat -> String -> Bool
palindrome n stot = let s = toLower stot
                    in length s > n && reverse s == s
