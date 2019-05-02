import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit $ unpack input
    then pure $ Just $ cast input
    else pure $ Nothing

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = let guess' = guess target (guesses + 1) in do
  putStrLn $ "Number Guesseds: " ++ show guesses
  putStr "Guess a number: "
  Just n <- readNumber
    | Nothing => do
        putStrLn "Invalid input"
        guess'
  if n == target
    then do putStrLn "You guess correctly"
    else do
      putStrLn "Try again"
      guess'

main : IO ()
main = do
  seed <- time
  n <- pure $ seed `mod` 100
  guess (cast n) 0
