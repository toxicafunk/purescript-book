module FileOperations where

import Data.Foldable
import Data.Maybe
import Prelude

import Control.MonadZero (guard)
import Data.Array (concatMap, cons, filter, length, null, (..), (:))
import Data.Array.Partial (head, tail)
import Data.Path (Path, filename, isDirectory, ls, root, size)
import Partial.Unsafe (unsafePartial)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

isEven :: Int -> Boolean
isEven n = mod n  2 == 0

countEven :: Array Int -> Int
countEven arr =
  if null arr
     then 0
     else if isEven (unsafePartial head arr)
             then 1 + countEven (unsafePartial tail arr)
             else countEven (unsafePartial tail arr)

square :: Array Int -> Array Int
square arr = (\n -> n * n) <$> arr

removeNegative :: Array Int -> Array Int
removeNegative arr = filter (\n -> n >= 0) arr

infix 2 filter as <$?>

removeNegative' :: Array Int -> Array Int
removeNegative' arr = (\n -> n >= 0) <$?> arr

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i,j]

isPrime :: Int -> Boolean
isPrime n = (length $ factors n) == 1

cartesianProduct :: Array Int -> Array Int -> Array (Array Int)
cartesianProduct a b = do
  i <- a
  j <- b
  pure [i,j]

triples :: Int -> Array (Array Int)
triples n = do
  i <- square $ 1 .. n
  j <- square $ 1 .. n
  let k = i + j
  guard $ k < n
  pure [i,j,k]

allTrue :: Array Boolean -> Boolean
allTrue arr = foldl (\acc b -> acc && b) true arr

count :: forall a. (a -> Boolean) -> Array a -> Int
count _ [] = 0
count p xs = if p (unsafePartial head xs)
               then count p (unsafePartial tail xs) + 1
               else count p (unsafePartial tail xs)

count' :: forall a. (a -> Boolean) -> Array a -> Int
count' p = loop 0
  where
    loop :: Int -> Array a -> Int
    loop acc [] = acc
    loop acc xs = if p (unsafePartial head xs)
                         then loop (acc+1) (unsafePartial tail xs)
                         else loop acc (unsafePartial tail xs)

reverse :: forall a. Array a -> Array a
reverse = foldl (\acc n -> cons n acc) []

filterPath ::  (Path -> Boolean) -> Path -> Array Path
filterPath f = filter (\p -> f p) <<< allFiles'

onlyFiles :: Path -> Array Path
onlyFiles file = filterPath (not isDirectory) file

findSmallestFile :: Path -> Maybe Path
findSmallestFile = foldl smallestFile Nothing <<< onlyFiles
  where
    smallestFile :: Maybe Path -> Path -> Maybe Path
    smallestFile Nothing y = Just y
    smallestFile (Just x) y =
      if size x < size y then Just x else Just y

findLargestFile :: Path -> Maybe Path
findLargestFile = foldl largestFile Nothing <<< onlyFiles
  where
    largestFile Nothing y = Just y
    largestFile (Just x) y =
      if size x > size y then Just x else Just y

whereIs :: String -> Maybe Path
whereIs name = safeMaybe $ do
  dir <- filterPath isDirectory root
  file <- ls dir
  guard $ filename file == name
  pure dir
  where
    safeMaybe :: Array Path -> Maybe Path
    safeMaybe [] = Nothing
    safeMaybe xs = Just (unsafePartial head xs)

