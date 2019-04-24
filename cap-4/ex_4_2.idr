import Data.Fin
import Data.Vect

data PowerSource = Petrol | Pedal | Eletric
data Cycle = Unicycles | Motorcycles

data Vehicle : PowerSource -> Cycle -> Type where
  Bicycle : Vehicle Pedal Motorcycles
  Car : (fuel : Nat) -> Vehicle Petrol Unicycles
  Bus : (fuel : Nat) -> Vehicle Petrol Unicycles

vectTake : (k : Fin (S n)) -> Vect n a -> Vect (finToNat k) a
vectTake FZ _ = []
vectTake (FS FZ) [] impossible
vectTake (FS (FS _)) [] impossible
vectTake (FS n) (x :: xs) = x :: vectTake n xs

sumEntriesHelper : Num a => (m : Fin n) -> (xs : Vect n a) -> (ys : Vect n a) -> Maybe a
sumEntriesHelper FZ (x :: _) (y :: _) = Just $ x + y
sumEntriesHelper (FS n) (_ :: xs) (_ :: ys) = sumEntriesHelper n xs ys

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys =
  case integerToFin pos n of
    Nothing => Nothing
    Just n => sumEntriesHelper n xs ys
