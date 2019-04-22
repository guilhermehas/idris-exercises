palindrome : String -> Bool
palindrome stot = let s = toLower stot
                    in length s > 10 && reverse s == s
