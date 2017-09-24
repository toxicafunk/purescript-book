module Exercises where

import Data.Array (foldM, fromFoldable, head, tail, sort, nub)
import Data.List
import Data.Maybe (Maybe)
import Prelude

import Control.Monad.Eff (Eff, forE, runPure)
import Control.Monad.ST (ST, newSTRef, readSTRef, modifySTRef, runST)
import Control.MonadZero

third :: forall a. Array a -> Maybe a
third arr = do
  ys <- tail arr
  xs <- tail ys
  head xs

sums :: Array Int -> Array Int
sums = nub <<< sort <<< foldM (\acc cur -> [acc, acc + cur]) 0

filterM' :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM' _ Nil = pure empty
filterM' f (x : xs) = f x >>=
  (\b -> filterM f xs >>= (\l -> pure $ if b then x : l else l)) 

-- filterM' :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM'' :: forall m a. MonadZero m => (a -> m Boolean) -> List a -> m (List a)
filterM'' _ Nil = pure empty
filterM'' f (x : xs) = do
  b <- f x
  guard b
  (pure (\l -> x : l)) <*> (filterM' f xs)

simulate :: forall eff h. Number -> Number -> Int -> Eff (st :: ST h | eff) Number
simulate x0 v0 time = do
  ref <- newSTRef { x: x0, v: v0 }
  forE 0 (time * 1000) \_ -> do
    _ <- modifySTRef ref (\o -> { v: o.v - 9.81 * 0.001, x: o.x + o.v * 0.001})
    pure unit
  final <- readSTRef ref
  pure final.x

simulate' :: Number -> Number -> Int -> Number
simulate' x0 v0 time = runPure $ runST (simulate x0 v0 time)
