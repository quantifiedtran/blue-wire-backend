module BlueWire.FRP where

import Prelude
import Data.Maybe
import Data.Time.Duration
import BlueWire.Types
import FRP as FRP
import FRP.Event as FRP
import FRP.Behavior (Behavior)
import Data.Function
import Control.Apply
import Control.Applicative
import Control.Monad

{-
-- |
kick :: forall dur. Duration dur => dur -> Behavior (Kick dur) -> FRP.Event Unit
kick dur ki =  <$>
-}
