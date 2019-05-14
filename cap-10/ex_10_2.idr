import Data.Vect
import Data.List.Views
import Data.Vect.Views
import Data.Nat.Views

equalSuffix : Eq a => List a -> List a -> List a
equalSuffix xs ys with (snocList xs)
  equalSuffix [] ys | Empty = []
  equalSuffix (zs ++ [x]) ys | (Snoc rec) with (snocList ys)
    equalSuffix (zs ++ [x]) [] | (Snoc rec) | Empty = []
    equalSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc recxs) | (Snoc reczs) =
      if x == y then
        let previousSuffix = equalSuffix xs ys | recxs | reczs
          in previousSuffix ++ [x]
      else []

mergeSort : Ord a => Vect n a -> Vect n a
mergeSort xs with (splitRec xs)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (xs ++ ys) | (SplitRecPair lrec rrec) = merge (mergeSort xs | lrec) (mergeSort ys | rrec)

toBinaryChar : Nat -> List Char
toBinaryChar n with (halfRec n)
  toBinaryChar Z | HalfRecZ = ['0']
  toBinaryChar (x + x) | (HalfRecEven rec) = (toBinary x | rec) ++ ['0']
  toBinaryChar (S (x + x)) | (HalfRecOdd rec) = if x == 0 then ['1'] else (toBinary x | rec) ++ ['1']

toBinary : Nat -> String
toBinary n = cast $ toBinaryChar n

palindrome : Eq a => List a -> Bool
palindrome xs with (vList xs)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (ys ++ [y])) | (VCons rec) = x == y && palindrome ys | rec

palindromeS : String -> Bool
palindromeS = palindrome . unpack
