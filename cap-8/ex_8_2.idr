import Data.Vect

sucright : (n : Nat) -> (m : Nat) -> S (n + m) = n + S m
sucright Z m = Refl
sucright (S k) m = cong (sucright k m)


myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z Z = Refl
myPlusCommutes Z (S m) = cong (myPlusCommutes Z m)
myPlusCommutes (S k) Z = cong (myPlusCommutes k Z)
myPlusCommutes (S k) (S j) = cong (trans (myPlusCommutes k (S j)) (sucright j k))

vectPlusLeft : {n : Nat} -> Vect n a -> Vect (n + 0) a
vectPlusLeft [] = []
vectPlusLeft (x :: xs) = x :: vectPlusLeft xs

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where
    reverse' : Vect n a -> Vect m a -> Vect (n + m) a
    reverse' {n = n} {m = Z} acc [] = rewrite myPlusCommutes n Z in acc
    reverse' {n = n} {m = S m} acc (x :: xs) = rewrite sym $ sucright n m in reverse' (x :: acc) xs
