module Test.Main where

import RandomChars

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (sort, sortBy, intersect, union, length, reverse, (:))
import Data.Foldable (all, foldr)
import Data.Function (on)
import Data.List (List(..), fromFoldable)
import Data.String (toCharArray)
import Merge (mergeWith, mergePoly, merge)
import Prelude (class Eq, class Ord, Unit, compare, discard, id, map, show, ($), (&&), (+), (<<<), (<=), (<>), (==), (||))
import Sorted (sorted)
import Test.QuickCheck (quickCheck, (<?>))
import Tree (Tree, member, insert, toArray, anywhere)

isSorted :: forall a. (Ord a) => Array a -> Boolean
isSorted = go <<< fromFoldable
  where
  go (Cons x1 t@(Cons x2 _)) = x1 <= x2 && go t
  go _ = true

isSubarrayOf :: forall a. Eq a => Array a -> Array a -> Boolean
isSubarrayOf xs ys = xs `intersect` ys == xs

isRespectsIdentity :: forall a. Eq a => Array a -> Boolean
isRespectsIdentity xs = xs `union` [] == xs

ints :: Array Int -> Array Int
ints = id

bools :: Array Boolean -> Array Boolean
bools = id

bool :: Boolean -> Boolean
bool = id

isSameLength :: forall a. Array a -> Array a -> Boolean
isSameLength xs ys = length xs == length ys

isOneMore :: forall a. Array a -> Array a -> Boolean
isOneMore xs ys = length xs == length ys + 1

intToBool :: (Int -> Boolean) -> Int -> Boolean
intToBool = id

treeOfInt :: Tree Number -> Tree Number
treeOfInt = id

isLower :: LowercaseString -> Boolean
isLower s = all (\c -> 'a' <= c && c <= 'z') (toCharArray $ show s)

main :: Eff ( console :: CONSOLE
            , random :: RANDOM
            , exception :: EXCEPTION
            ) Unit
main = do
  -- Tests for module 'Merge'

  quickCheck $ \xs ys -> isSorted $ merge (sorted xs) (sorted ys)
  quickCheck $ \xs ys -> xs `isSubarrayOf` merge xs ys

  quickCheck \xs ys ->
    let result = merge (sort xs) (sort ys)
    in
       xs `isSubarrayOf` result <?> show xs <> " not a subarray of " <> show result

  quickCheck \xs ->
    let result = merge xs []
    in
     isRespectsIdentity xs <?> show xs <> " does not respects identity " <> show result

  quickCheck $ \xs ys -> isSorted $ ints $ mergePoly (sorted xs) (sorted ys)
  quickCheck $ \xs ys -> ints xs `isSubarrayOf` mergePoly xs ys


  quickCheck $ \xs ys -> isSorted $ bools $ mergePoly (sorted xs) (sorted ys)
  quickCheck $ \xs ys -> bools xs `isSubarrayOf` mergePoly xs ys

  quickCheck $ \xs ->
    let ys = ints xs
        in ys `isSameLength` reverse ys
  quickCheck $ \x xs ->
    let ys  = bools xs
        in (x : ys) `isOneMore` ys

  quickCheck $ \xs ys f -> isSorted $ map f $ mergeWith (intToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)
  quickCheck $ \xs ys f -> xs `isSubarrayOf` mergeWith (intToBool f) xs ys

  quickCheck \xs ys ->
    let result = lowerCharArrayToLowerString $ mergePoly (chars xs) (chars ys)
        in
         isLower result <?> show result <> " is not lowercase " <> show xs <> " " <> show ys

  -- Tests for module 'Tree'

  quickCheck $ \t a -> member a $ insert a $ treeOfInt t
  quickCheck $ \t xs -> isSorted $ toArray $ foldr insert t $ ints xs

  quickCheck $ \f g t ->
    anywhere (\s -> f s || g s) t ==
      anywhere f (treeOfInt t) || anywhere g t
