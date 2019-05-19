import Control.Monad.State

data Tree a = Empty
            | Node (Tree a) a (Tree a)

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

-- ex 1 --

update : (stateType -> stateType) -> State stateType ()
update f = do
  state <- get
  put $ f state

increase : Nat -> State Nat ()
increase k = update (+k)

-- ex 2 --

countEmpty : Tree a -> State Nat ()
countEmpty Empty = increase 1
countEmpty (Node left _ right) = do
  countEmpty left
  countEmpty right

countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = do
  (empty, nodes) <- get
  put (empty + 1, nodes)
countEmptyNode (Node left _ right) = do
  countEmptyNode left
  (empty, nodes) <- get
  put $ (empty, nodes + 1)
  countEmptyNode right
