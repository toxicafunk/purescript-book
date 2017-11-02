module SafeDivide where

import Control.Monad.Error.Class
import Control.Monad.Except.Trans
import Data.Identity
import Data.Either
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

safeDivide :: Number -> Number -> ExceptT String Identity Number
safeDivide x 0.0 = throwError "Divide by 0!"
safeDivide x y = pure $ x / y

-- unwrap $ runExceptT $ safeDivide 20.0 5.0

main :: forall e. Number -> Number -> Eff (console :: CONSOLE | e) Unit
main x y = do
  log $ case i of
            Identity (Right r) -> show r
            Identity (Left s) -> s
       where i = runExceptT $ safeDivide x y
