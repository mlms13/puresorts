module MergeSort where

import MyList
import Prelude

sort :: ∀ a. Ord a => MyList a -> MyList a
sort Nil = Nil
sort (a : Nil) = a : Nil
sort list =
  merge (sort left) (sort right)
  where
    { left, right } = bisect list


merge :: ∀ a. Ord a => MyList a -> MyList a -> MyList a
merge la lb =
  merge' la lb Nil
  where
    merge' :: ∀ x. Ord x => MyList x -> MyList x -> MyList x -> MyList x
    merge' a Nil acc = acc <> a
    merge' Nil b acc = acc <> b
    merge' (a : as) ball @ (b : _) acc | a < b = a : merge as ball
    merge' aall (b : bs) acc = b : merge bs aall
