module MyList where

import Data.Foldable (class Foldable, foldl, foldr)
import Data.Monoid (class Monoid, mempty)

import Prelude (class Show, show, (<>))


data MyList a
  = Nil
  | Cons a (MyList a)

mine :: MyList Int
mine = Cons 1 (Cons 2 (Cons 3 Nil))

-- bisect :: ∀ a. MyList a -> { left :: MyList a, right :: MyList a }
-- bisect =
--   foldl (\acc curr -> acc) { left = Nil, right = Nil}


-- No need to implement length... we get it for free from Data.Foldable
-- length :: ∀ a. MyList a -> Int
-- length = unwrap <<< foldMap (const $ Additive 1)

instance showMyList :: Show a => Show (MyList a) where
  show l =
    "[" <> (show' l) <> "]" where
      show' :: ∀ b. Show b => MyList b -> String
      show' Nil = ""
      show' (Cons a Nil) = show a
      show' (Cons a list) = show a <> "," <> show' list

instance foldMyList :: Foldable MyList where
  foldl :: ∀ a b. (b -> a -> b) -> b -> MyList a -> b
  foldl f init Nil = init
  foldl f init (Cons a list) =
    foldl f (f init a) list

  foldr :: ∀ a b. (a -> b -> b) -> b -> MyList a -> b
  foldr f init Nil = init
  foldr f init (Cons a list) =
    f a (foldr f init list)

  foldMap :: ∀ a m. Monoid m => (a -> m) -> MyList a -> m
  foldMap f list = foldl (\b a -> b <> f a) mempty list
