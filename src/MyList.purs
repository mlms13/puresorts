module MyList where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Monoid (class Monoid, mempty)

data MyList a
  = Nil
  | Cons a (MyList a)

infixr 6 Cons as :

mine :: MyList Int
mine = 1 : 2 : 3 : nil

nil :: ∀ a. MyList a
nil = Nil

filter :: ∀ a. (a -> Boolean) -> MyList a -> MyList a
filter p =
  foldl (\acc curr -> if p curr then curr : acc else acc) nil

bisect :: ∀ a. MyList a -> { left :: MyList a, right :: MyList a }
bisect l =
  { left : f.left, right: f.right }
  where
    f = foldl split { left : Nil, right : Nil, toLeft : true } l

    split { left, right, toLeft : true } a =
      { left : (a : left), right, toLeft : false }
    split { left, right, toLeft : false } a =
      { left, right : (a : right), toLeft : true }


-- No need to implement length... we get it for free from Data.Foldable
-- length :: ∀ a. MyList a -> Int
-- length = unwrap <<< foldMap (const $ Additive 1)

instance showMyList :: Show a => Show (MyList a) where
  show l =
    "[" <> (show' l) <> "]" where
      show' :: ∀ b. Show b => MyList b -> String
      show' Nil = ""
      show' (a : Nil) = show a
      show' (a : list) = show a <> "," <> show' list

instance semiGroupMyList :: Semigroup (MyList a) where
  append :: ∀ a. MyList a -> MyList a -> MyList a
  append a Nil = a
  append Nil b = b
  append (a : Nil) b = a : b
  append (a : resta) b = a : append resta b

instance monoidMyList :: Monoid (MyList a) where
  mempty = Nil

derive instance functorMyList :: Functor MyList


instance applyMyList :: Apply MyList where
  apply :: ∀ a b. MyList (a -> b) -> MyList a -> MyList b
  apply Nil _ = Nil
  apply (f : fns) list = (f <$> list) <> (fns <*> list)


instance applicativeMyList :: Applicative MyList where
  pure a = a : Nil

instance bindMyList :: Bind MyList where
  bind :: ∀ a b. MyList a -> (a -> MyList b) -> MyList b
  bind list fn =
    foldMap fn list


instance foldMyList :: Foldable MyList where
  foldl :: ∀ a b. (b -> a -> b) -> b -> MyList a -> b
  foldl f init Nil = init
  foldl f init (a : list) =
    foldl f (f init a) list

  foldr :: ∀ a b. (a -> b -> b) -> b -> MyList a -> b
  foldr f init Nil = init
  foldr f init (a : list) =
    f a (foldr f init list)

  foldMap :: ∀ a m. Monoid m => (a -> m) -> MyList a -> m
  foldMap f list = foldl (\b a -> b <> f a) mempty list

-- instance arbMyList :: Arbitrary a => Arbitrary (MyList a) where
--   arbitrary = gen ?hole
