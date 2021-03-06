module Main where

import Data.Maybe
import Data.Monoid
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Array ((:))
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Partial.Unsafe (unsafePartial)

newtype Complex = Complex
   { real :: Number
   , imaginary :: Number
   }

instance showComplex :: Show Complex where
  show (Complex {real, imaginary}) = show real <> " i" <> show imaginary

instance eqComplex :: Eq Complex where
  eq (Complex {real: r1, imaginary: i1}) (Complex {real: r2, imaginary: i2}) = r1 == r2 && i1 == i2

data NonEmpty a = NonEmpty a (Array a)

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty x xs) = show (x : xs)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  --eq :: NonEmpty a -> NonEmpty a -> Boolean
  eq (NonEmpty x xs) (NonEmpty y ys) = x == y && xs == ys

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> (y : ys))

instance functorNonEmpty :: Functor NonEmpty where
  -- map :: forall a b. (a -> b) -> f a -> f b
  map f (NonEmpty x xs) = NonEmpty (f x) (f <$> xs)

data Extended a = Finite a | Infinite

instance showExtended :: Show a => Show (Extended a) where
  show Infinite = "∞"
  show (Finite x) = "Finite " <> show x

instance eqExtended :: Eq a => Eq (Extended a) where
  -- eq :: a -> a -> Boolean
  eq Infinite Infinite = true
  eq (Finite x) (Finite y) = x == y
  eq Infinite (Finite y) = false
  eq (Finite x) Infinite = false

instance ordExtended :: Ord a => Ord (Extended a) where
  -- compare :: a -> a -> Ordering
  compare Infinite Infinite = EQ
  compare Infinite (Finite y) = GT
  compare (Finite x) Infinite = LT
  compare (Finite x) (Finite y) = compare x y

instance foldableNonEmpty :: Foldable NonEmpty where
  -- foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldr f y (NonEmpty x xs) = foldr f y (x : xs)
-- foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldl f y (NonEmpty x xs) = foldl f y (x : xs)
-- foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
  foldMap f (NonEmpty x xs) = foldMap f (x : xs)

data OneMore f a = OneMore a (f a)

instance showOneMore :: (Show (f a), Show a) => Show (OneMore f a) where
  show :: (OneMore a f) -> String
  show (OneMore x f) = show x <> " + " <> show f

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldl g y (OneMore x f) = foldl g (g y x) f
  foldr g y (OneMore x f) = foldr g (g x y) f
  foldMap g (OneMore x f) = g x <> foldMap g f

maxInt :: Partial => Array Int -> Int
maxInt arr = case maximum arr of
  Just x -> x

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

class Monoid m <= Action m a where
   act :: m -> a -> a

instance repeatAction :: Action Multiply String where
  act (Multiply n) s = repeat n s where
    repeat 0 _ = mempty
    repeat n s = s <> repeat (n-1) s

instance arrayAction :: Action m a => Action m (Array a) where
  act m arr = map (act m) arr

-- newtype Self m = Self m

-- instance semigroupSelf :: Semigroup m => Semigroup (Self m) where
--  append (Self s1) (Self s2) = Self(s1 <> s2)

-- instance monoidSelf :: Monoid (Self m) where
--  mempty = Self

-- instance selfAction :: Action m (Self m) where
--  act m s = foldl (<>) (mempty s) s

ext1 :: Extended Int
ext1 = Finite 2

ext4 :: Extended Int
ext4 = Infinite

ne1 :: NonEmpty Int
ne1 = NonEmpty 2 [3,4,5]

om1 :: OneMore Array Int
om1 = OneMore 1 [2,3]

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  -- log (show ext4 <> show ext1)
  logShow (foldl (\acc x -> acc + x) 0 ne1)
  logShow (foldl (\acc x -> acc + x) 0 om1)
  logShow (foldr (\x acc -> acc + x) 0 om1)
  log (foldMap show om1)
  logShow (unsafePartial maxInt [3,7,8,2,3])
  log (act (Multiply 3) "Eo")
