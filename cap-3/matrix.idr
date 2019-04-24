import public Data.Vect

Matrix : Nat -> Nat -> Type -> Type
Matrix n m a = Vect n (Vect m a)

empties : (n : Nat) -> Matrix n 0 a
empties Z = []
empties (S k) = [] :: empties k

transposeMat : Matrix n m a -> Matrix m n a
transposeMat {m = k} [] = empties k
transposeMat mat@([] :: xs) = []
transposeMat mat@((x :: ys) :: xs) = map head mat :: transposeMat (map tail mat)

addMatrix : Num a => Matrix n m a-> Matrix n m a -> Matrix n m a
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

multMatrix : Num a => Matrix n m a -> Matrix m p a -> Matrix n p a
multMatrix xs yss = let ys = transpose yss
                    in map (f ys) xs
  where
    joinVectors : Vect m a -> Vect m a -> a
    joinVectors xs ys = sum $ zipWith (*) xs ys

    f : Vect p (Vect m a) -> Vect m a -> Vect p a
    f xs ys = map (joinVectors ys) xs
