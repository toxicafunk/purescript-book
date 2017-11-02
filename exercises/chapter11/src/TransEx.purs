module TransEx where

import Control.Monad.Error.Class
import Control.Monad.Except.Trans
import Control.Monad.State.Trans
import Control.Monad.Writer.Trans
import Control.MonadPlus
import Data.Array
import Data.Either
import Data.Identity
import Data.Maybe
import Data.Newtype
import Prelude

import Data.String (Pattern(..), drop, stripPrefix, take, toLower, toUpper)

split :: StateT String (Either String) String
split = do
  s <- get
  case s of
    "" -> lift $ Left "Empty String"
    _ -> do
      put (drop 1 s)
      pure (take 1 s)

type Errors = Array String

type Log = Array String

type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

split' :: Parser String
split' = do
  s <- get
  lift $ tell ["The state is " <> show s]
  case s of
    "" -> lift $ lift $ throwError ["Empty string"]
    _ -> do
      put (drop 1 s)
      pure (take 1 s)

runParser p s = unwrap $ runExceptT $ runWriterT $ runStateT p s
--runParser ((<>) <$> split' <*> split') "test"

string :: String -> Parser String
string prefix = do
  s <- get
  lift $ tell ["The state is " <> show s]
  case (stripPrefix (Pattern prefix) s) of
    Nothing -> lift $ lift $ throwError ["Prefix not found"]
    (Just suffix) -> do
                     put suffix
                     pure prefix

-- runParser (string "abc") "abcdef"

string' :: String -> Parser String
string' prefix = do
  s <- get
  tell ["The state is " <> show s]
  case (stripPrefix (Pattern prefix) s) of
    Nothing -> throwError ["Prefix not found"]
    (Just suffix) -> do
                     put suffix
                     pure prefix

upper :: Parser String
upper = do
  s <- split'
  guard $ toUpper s == s
  pure s

lower :: Parser String
lower = do
  s <- split'
  guard $ toLower s == s
  pure s

upperOrLower = some upper <|> some lower
components = many upperOrLower

asOrBs = some (string "a") <|> some (string "b")
asAndBs = many asOrBs
