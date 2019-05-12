data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

Eq Shape where
  (==) (Triangle x z) (Triangle y w) = x == y && w == z
  (==) (Rectangle x z) (Rectangle y w) = x == y && w == z
  (==) (Circle x) (Circle y) = x == y
  (==) _ _ = False

area : Shape -> Double
area (Triangle b h) = b * h / 2
area (Rectangle b h) = b * h
area (Circle r) = 3.14 * r * r

Ord Shape where
  compare s s' = compare (area s) (area s')

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]
