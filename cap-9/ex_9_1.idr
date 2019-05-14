import Data.List

notInEmpty : Elem value [] -> Void
notInEmpty Here impossible
notInEmpty (There _) impossible

notInRest : (notThere : Elem value xs -> Void) -> (notHere : (value = x) -> Void) -> Elem value (x :: xs) -> Void
notInRest notThere notHere Here = notHere Refl
notInRest notThere notHere (There later) = notThere later

isElem : DecEq  a => (value : a) -> (xs : List a) -> Dec (Elem value xs)
isElem value [] = No notInEmpty
isElem value (x :: xs)
  = case decEq value x of
      Yes Refl => Yes Here
      No notHere => case Main.isElem value xs of
                  Yes prf => Yes $ There prf
                  No notThere => No $ notInRest notThere notHere

data Last : List a -> a -> Type where
  LastOne : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

last123 : Last [1,2,3] 3
last123 = LastCons (LastCons LastOne)

noLastEmpty : Last [] value -> Void
noLastEmpty LastOne impossible
noLastEmpty (LastCons _) impossible

notLastEmpty : (no : (x = value) -> Void) -> Last [x] value -> Void
notLastEmpty no LastOne = no Refl
notLastEmpty _ (LastCons LastOne) impossible
notLastEmpty _ (LastCons (LastCons _)) impossible


noLastTail : (notLast : Last (y :: ys) value -> Void) -> Last (x :: (y :: ys)) value -> Void
noLastTail notLast (LastCons isLast) = notLast isLast

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No noLastEmpty
isLast (x :: []) value
 = case decEq x value of
     Yes Refl => Yes LastOne
     No no => No (notLastEmpty no)
isLast (x :: (y :: ys)) value
  = case isLast (y :: ys) value of
      Yes LastOne => Yes $ LastCons LastOne
      Yes (LastCons prf) => Yes $ LastCons $ LastCons prf
      No notLast => No (noLastTail notLast)
