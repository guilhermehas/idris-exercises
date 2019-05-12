data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

Num lit => Num (Expr lit) where
  (+) x y = Add x y
  (*) x y = Mul x y
  fromInteger = Val . fromInteger

Show lit => Show (Expr lit) where
	show (Val lit) = show lit
	show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
	show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
	show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
	show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
	show (Abs x)   = "|" ++ show x ++ "|"

||| Evaluate using `Neg`/`Integral` operations.
eval : (Neg lit, Ord lit, Integral lit) => Expr lit -> lit
eval (Val lit) = lit
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = let ex = eval x in (if ex > 0 then ex else - ex)

(Eq lit, Neg lit, Integral lit, Ord lit) => Eq (Expr lit) where
  (==) x y = eval x == eval y

(Neg lit, Ord lit, Integral lit) => Cast (Expr lit) lit where
  cast = eval

Functor Expr where
  map func (Val x) = Val $ func x
  map func (Add x y) = Add (map func x) (map func y)
  map func (Sub x y) = Sub (map func x) (map func y)
  map func (Mul x y) = Mul (map func x) (map func y)
  map func (Div x y) = Div (map func x) (map func y)
  map func (Abs x) = Abs $ map func x
