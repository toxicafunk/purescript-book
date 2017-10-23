module TransEx where

import Prelude

import Data.String (drop, take)
import Data.Either
import Control.Monad.State.Trans

split :: StateT String (Either String) String
split = do
  s <- get
  case s of
    "" -> lift $ Left "Empty String"
    _ -> do
      put (drop 1 s)
      pure (take 1 s)
