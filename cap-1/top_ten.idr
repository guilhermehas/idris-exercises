top_ten : Ord a => List a -> List a
top_ten numbers = take 10 $ reverse $ sort numbers
