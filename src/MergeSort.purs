module MergeSort where

import MyList
import Prelude

sort :: ∀ a. Ord a => MyList a -> MyList a
sort Nil = Nil
sort (Cons a Nil) = a : nil
sort list =
  merge (sort left) (sort right)
  where
    { left, right } = bisect list


merge :: ∀ a. Ord a => MyList a -> MyList a -> MyList a
merge a b =
  merge' a b Nil
  where
    merge' :: ∀ a. Ord a => MyList a -> MyList a -> MyList a -> MyList a
    merge' a Nil acc = acc <> a
    merge' Nil b acc = acc <> b
    merge' aall @ (Cons a arest) ball @ (Cons b brest) acc =
      case compare a b of
        LT -> acc <> (Cons a $ merge arest ball)
        GT -> acc <> (Cons b $ merge brest aall)
        EQ -> acc <> (Cons a $ merge arest ball)
