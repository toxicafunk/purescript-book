module Main where

import Control.Apply

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr, sequence, traverse)
import Prelude (class Applicative, class EuclideanRing, class Functor, class Ring, class Semigroup, class Semiring, class Show, Unit, discard, pure, show, ($), (*), (+), (-), (/), (<$>), (<*>), (<>))

maybeAdd :: forall a. Semiring a => Maybe a -> Maybe a-> Maybe a
maybeAdd x y = lift2 (+) x y

maybeSub :: forall a. Ring a => Maybe a -> Maybe a-> Maybe a
maybeSub x y = lift2 (-) x y

maybeMultiply :: forall a. Semiring a => Maybe a -> Maybe a-> Maybe a
maybeMultiply x y = lift2 (*) x y

maybeDiv :: forall a. EuclideanRing a => Maybe a -> Maybe a-> Maybe a
maybeDiv x y = lift2 (/) x y

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing  = pure Nothing
combineMaybe (Just fa) = Just <$> fa

ft1 :: Maybe (Array Int)
ft1 = Just [1,2,3]

data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)

instance treeShow :: Show a => Show (Tree a) where
  show Leaf = "\n*\n"
  show (Branch l v r) = show l <> " | " <> show v <> " | " <> show r

instance treeSemigroup :: Semigroup a => Semigroup (Tree a) where
  append :: Tree a -> Tree a -> Tree a
  append Leaf Leaf = Leaf
  append (Branch l v r) Leaf = Branch l v r
  append Leaf (Branch l v r) = Branch l v r
  append (Branch l1 v1 r1) (Branch l2 v2 r2) = Branch (l1 <> l2) (v1 <> v2) (r1 <> r2)

instance treeMonoid :: Monoid a => Monoid (Tree a) where
  mempty = Leaf

instance treeFoldable :: Foldable Tree where
  foldl :: forall a b. (b -> a -> b) -> b -> Tree a -> b
  foldl f y Leaf = y
  foldl f y (Branch l v r) = foldl f (f (foldl f y l) v) r

  foldr :: forall a b. (a -> b -> b) -> b -> Tree a -> b
  foldr f y Leaf = y
  foldr f y (Branch l v r) = foldr f (f v (foldr f y l)) r

  foldMap :: forall a m. Monoid m => (a -> m) -> Tree a -> m
  foldMap f Leaf = mempty
  foldMap f (Branch l v r) = (foldMap f l) <> (f v) <> (foldMap f r)

instance treeFunctor :: Functor Tree where
  map :: forall a b. (a -> b) -> Tree a -> Tree b
  map f Leaf = Leaf
  map f (Branch l v r) = Branch (f <$> l) (f v) (f <$> r)

instance treeTraversable :: Traversable Tree where
  traverse :: forall a b f. Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g Leaf = pure Leaf
  traverse g (Branch l v r) =
    Branch <$> (traverse g l)
           <*> (g v)
           <*> (traverse g r)

  sequence :: forall a f. Applicative f => Tree (f a) -> f (Tree a)
  sequence Leaf = pure Leaf
  sequence (Branch lx v rx) =
    Branch <$> (sequence lx)
           <*> v
           <*> (sequence rx)

b1 :: Tree Int
b1 = Branch Leaf 5 Leaf

b2 :: Tree Int
b2 = Branch b1 6 (Branch Leaf 10 Leaf)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ maybeAdd (Just 12) (Just 20)
  logShow $ maybeSub (Just 32) (Just 20)
  logShow $ maybeMultiply (Just 12) (Just 20)
  logShow $ maybeDiv (Just 20) (Just 10)
  logShow $ combineMaybe ft1
  logShow b1
  logShow b2
