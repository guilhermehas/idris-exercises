import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Double)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

data Format =
    Integ
  | Str
  | Doubles
  | Lit String

PrintFormat : List Format -> Type
PrintFormat [] = String
PrintFormat (Integ :: xs) = Int -> PrintFormat xs
PrintFormat (Str :: xs) = String -> PrintFormat xs
PrintFormat (Doubles :: xs) = Double -> PrintFormat xs
PrintFormat ((Lit _) :: xs) = PrintFormat xs

toFormat : List Char -> List Format
toFormat [] = []
toFormat (x :: []) = [Lit (show x)]
toFormat ('%' :: ('s' :: xs)) = Str :: toFormat xs
toFormat ('%' :: ('f' :: xs)) = Doubles :: toFormat xs
toFormat ('%' :: ('d' :: xs)) = Integ :: toFormat xs
toFormat (x :: (y :: ys)) = case toFormat (y :: ys) of
                                 [] => [Lit (show x)]
                                 (Lit ws) :: zs => Lit (show x ++ ws) :: zs
                                 z :: zs => Lit (show x) :: z :: zs

printAcc : (form : List Format) -> String -> PrintFormat form
printAcc [] acc = acc
printAcc (Integ :: xs) acc = \n => printAcc xs (acc ++ show n)
printAcc (Str :: xs) acc = \s => printAcc xs (acc ++ s)
printAcc (Doubles :: xs) acc = \d => printAcc xs (acc ++ show d)
printAcc ((Lit ys) :: xs) acc = printAcc xs (acc ++ ys)

printf : (s : String) -> PrintFormat (toFormat $ unpack s)
printf s = printAcc (toFormat $ unpack s) ""


TupleVect : Nat -> Type -> Type
TupleVect Z type = Unit
TupleVect (S k) type = Pair type (TupleVect k type)

test : TupleVect 4 Nat
test = (1,2,3,4,())
