module WriterEx where

import Control.Monad.Writer
import Control.Monad.Writer.Class
import Data.Monoid.Additive
import Prelude

import Data.Traversable (traverse_)

gcdLog :: Int -> Int -> Writer (Array String) Int
gcdLog n 0 = pure n
gcdLog 0 m = pure m
gcdLog n m = do
  tell ["gcdLog " <> show n <> " " <> show m]
  if n > m
    then gcdLog (n - m) m
    else gcdLog n (m - n)

sumArray :: Array Number -> Writer (Additive Number) Unit
sumArray = traverse_ \n -> tell (Additive n)

countCollatz :: Int -> Array String
countCollatz = execWriter <<< count 0 
  where
    count :: Int -> Int -> Writer (Array String) Int
    count i 1 = pure i
    count i x = do
      tell ["f(" <> show x <> ")"]
      count (i + 1) (collatz x) 
      where
        collatz :: Int -> Int
        collatz n | n `mod` 2 == 0 = n / 2
        collatz n                  = 3 * n + 1


