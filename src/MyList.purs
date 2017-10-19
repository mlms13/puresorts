module MyList where

import Data.Foldable (class Foldable, foldl, foldr)
import Data.Monoid (class Monoid, mempty)

import Prelude

data MyList a
  = Nil
  | Cons a (MyList a)

mine :: MyList Int
mine = 1 : 2 : 3 : nil

cons :: ∀ a. a -> MyList a -> MyList a
cons = Cons

nil :: ∀ a. MyList a
nil = Nil

infixr 6 cons as :

bisect :: ∀ a. MyList a -> { left :: MyList a, right :: MyList a }
bisect l =
  { left : f.left, right: f.right }
  where
    f = foldl split { left : Nil, right : Nil, toLeft : true } l
    split { left, right, toLeft : true } a =
      { left : Cons a left, right, toLeft : false }
    split { left, right, toLeft : false } a =
      { left, right : Cons a right, toLeft : true }


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

instance semiGroupMyList :: Semigroup (MyList a) where
  append :: ∀ a. MyList a -> MyList a -> MyList a
  append a Nil = a
  append Nil b = b
  append (Cons a Nil) b =
    Cons a b
  append (Cons a resta) b =
    Cons a $ append resta b


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
