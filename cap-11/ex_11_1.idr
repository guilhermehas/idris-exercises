-- ex 1 --

every_other : Stream a -> Stream a
every_other (_ :: (x :: xs)) = x :: every_other xs

---

data InfList : Type -> Type where
  (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

getPrefix : (count : Nat) -> InfList a -> List a
getPrefix Z xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs

countFrom : Integer -> InfList Integer
countFrom x = x :: countFrom (x + 1)

-- ex 2 --

Functor InfList where
  map func (value :: xs) = func value :: map func xs

---

export
randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                  (seed' `shiftR` 2) :: randoms seed'

-- ex 3 --

data Face = Heads | Tails

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips count xs = take count $ map toFace xs
  where
    toFace : Int -> Face
    toFace n = if n `div` 2 == 0 then Heads else Tails

-- ex 4 --

square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx = approx :: square_root_approx number (next number approx)
  where
    next : Double -> Double -> Double
    next number approx = (approx + (number / approx)) / 2

-- ex 5 --

square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) ->
                    (approxs : Stream Double) -> Double
square_root_bound Z _ _ (approx :: _) = approx
square_root_bound (S k) number bound (approx :: approxs)
 = if abs (number - approx * approx) < bound
     then approx
     else square_root_bound k number bound approxs

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.00000000001
                                  (square_root_approx number number)
