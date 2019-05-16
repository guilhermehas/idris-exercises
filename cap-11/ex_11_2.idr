data InfIO : Type where
  Do : IO a
  -> (a -> Inf InfIO)
  -> InfIO

(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

forever : Fuel
forever = More forever

run : Fuel -> InfIO -> IO ()
run (More fuel) (Do c f) = do res <- c
                              run fuel (f res)
run Dry p = putStrLn "Out of fuel"


totalREPL : (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt action = do ioStart
                             totalREPL prompt action
  where
    ioStart : IO ()
    ioStart = do putStr prompt
                 digited <- getLine
                 putStrLn $ action digited
