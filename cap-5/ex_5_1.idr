longerStringSize : String -> String -> Nat
longerStringSize s1 s2 = max (length s1) (length s2)

printLonger : IO ()
printLonger = do
  putStr "First string: "
  s1 <- getLine
  putStr "Second string: "
  s2 <- getLine
  putStrLn $ show $ longerStringSize s1 s2
