module Control.Monad.Eff.Alert where

import Prelude

import Control.Monad.Eff (kind Effect, Eff)

foreign import data ALERT :: Effect

foreign import alert :: forall eff. String -> Eff (alert :: ALERT | eff) Unit

foreign import confirm :: forall eff. String -> Eff (alert :: ALERT | eff) Boolean

