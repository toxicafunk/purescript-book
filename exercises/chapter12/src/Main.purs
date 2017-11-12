module Main' where

import Control.Monad.Cont.Trans (runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Prelude (Unit)
import Timeout (TIMEOUT, setTimeoutCont)

main :: Eff ( timeout :: TIMEOUT
            , console :: CONSOLE
            ) Unit
main = runContT (setTimeoutCont 2000) \_ -> log "Done!"
