palindrome : String -> Bool
palindrome stot = let s = toLower stot
                    in reverse s == s
