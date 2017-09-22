module Excercises where

import Data.Array (foldM, fromFoldable, head, tail, sort, nub)
import Data.List
import Data.Maybe (Maybe)
import Prelude

import Control.Monad.Eff (Eff)
import Control.MonadZero

third :: forall a. Array a -> Maybe a
third arr = do
  ys <- tail arr
  xs <- tail ys
  head xs

sums :: Array Int -> Array Int
sums = nub <<< sort <<< foldM (\acc cur -> [acc, acc + cur]) 0

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure empty
filterM f (x : xs) = f x >>=
  (\b -> filterM f xs >>= (\l -> pure $ if b then x : l else l)) 
