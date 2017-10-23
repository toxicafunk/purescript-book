module Exercises where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.State.Class
import Data.String
import Data.Traversable
import Prelude

import Data.Foldable (traverse_)

sumArray :: Array Number -> State Number Unit
sumArray = traverse_ \n -> modify \sum -> sum + n

testParens :: String -> Boolean
testParens chars = (execState (balance $ toCharArray chars) 0) == 0
  where
    balance :: Array Char -> State Int Unit
    balance = traverse_ \c -> modify \sum -> if c == '(' then sum + 1 else sum - 1

type Level = Int

type Doc = Reader Level String


repeat :: Int -> String -> String
repeat lvl s = if (lvl == 0)
                 then s
                 else s <> (repeat (lvl - 1) s)

line :: String -> Doc
line str = do
  lvl <- ask
  pure $ (repeat lvl " ") <> str

indent :: Doc -> Doc
indent = local \ind -> ind + 1

cat :: Array Doc -> Doc
cat docs = joinWith "\n" <$> sequence docs 

render :: Doc -> String
render doc = runReader doc 0

readerFunction :: Int -> Int
readerFunction = do
  x <- ask
  y <- pure (x + 3)
  pure (x + y + 2)

hello :: Reader String String
hello = do
  name <- ask
  pure ("hello, " <> name <> "!")

bye :: Reader String String
bye = do
  name <- ask
  pure ("bye, " <> name <> "!")

convo :: Reader String String
convo = do
  c1 <- hello
  c2 <- bye
  pure $ c1 <> c2

--show <<< runReader convo $ "adit"
