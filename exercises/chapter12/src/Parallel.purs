module Parallel where

import Prelude
import Control.Apply (lift2)
import Control.Monad.Cont.Trans (runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Parallel (parallel, sequential)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)

import Data.Either
import Types
import Files
import Network.HTTP.Client

main :: forall eff. Eff (console :: CONSOLE
                        , fs :: FS
                        | eff) Unit
main = flip runContT logShow do
  sequential $
    lift2 append
      <$> parallel (readFileCont "/tmp/1.txt")
      <*> parallel (readFileCont "/tmp/2.txt")

concatFile :: forall eff. Eff (console :: CONSOLE
                        , fs :: FS
                        | eff) Unit
concatFile = flip runContT logShow (runExceptT  do
  sequential $
      )

concatRequests :: forall eff. URI -> URI -> Eff (console :: CONSOLE
                                                , http :: HTTP
                                                | eff) Unit
concatRequests req1 req2 = flip runContT logShow do
  sequential $
    lift2 append
    <$> parallel (get req1)
    <*> parallel (get req2)
