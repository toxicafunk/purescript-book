module Timeout where

import Prelude

import Control.Monad.Cont.Trans (ContT(..), runContT)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console (error, logShow, log)
import Data.Function.Uncurried (Fn2, runFn2)
import Types (Async)

type Milliseconds = Int

foreign import data TIMEOUT :: Effect

foreign import setTimeoutImpl :: forall eff a. Fn2
      Milliseconds
      (Eff a Unit)
      (Eff (timeout :: TIMEOUT | eff) Unit)

setTimeout' :: forall eff a. Milliseconds ->
      (Eff a Unit) ->
      Eff (timeout :: TIMEOUT | eff) Unit
setTimeout' millis fn = runFn2 setTimeoutImpl millis fn
-- type Async eff = ContT Unit (Eff eff)

setTimeoutCont :: forall eff. Milliseconds -> Async (timeout :: TIMEOUT | eff) Unit
setTimeoutCont millis = ContT \cont -> setTimeout' millis (cont unit)

example = --do
  setTimeoutCont 2000
  --pure $ log "Hi!"

--main = runContT example \_ -> log "Done"
main = runContT (setTimeoutCont 2000) \_ -> log "Done!"
