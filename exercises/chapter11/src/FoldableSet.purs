module FoldableSet where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Foldable (fold)

import Data.Set
import Data.Foldable

isValid :: Int -> Boolean
isValid i = member i theSet
  where theSet = fromFoldable [1,2,3,4]

isValid' = \i -> member i theSet
  where theSet = fromFoldable [1,2,3,4]
