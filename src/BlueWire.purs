module BlueWire where

import BlueWire.Types
import BlueWire.Crypto
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad
import Data.Unit

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = log "don't cut the blue wire!!"

{-
bluewire :: forall inp. BlueWireConf -> { timePassed :: Unit | inp} ->
bluewire _ _ =
-}
