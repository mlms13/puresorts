module TreeSort where

import Data.Foldable (foldl)
import MyList
import Prelude

data Tree a
  = Empty
  | Node (Tree a) a (Tree a)

insert :: ∀ a. Ord a => a -> Tree a -> Tree a
insert a Empty = Node Empty a Empty
insert a (Node l x r) | a <= x = Node (insert a l) x r
insert a (Node l x r) = Node l x (insert a r)

flatten :: ∀ a. Tree a -> MyList a
flatten Empty = Nil
flatten (Node l x r) = (flatten l) <> (x : Nil) <> (flatten r)

sort :: ∀ a. Ord a => MyList a -> MyList a
sort Nil = Nil
sort (x : Nil) = (x : Nil)
sort list =
  flatten $ foldl (flip insert) Empty list
