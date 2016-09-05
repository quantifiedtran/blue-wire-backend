module BlueWire.Filesystem where

import Control.Monad.Eff
import DOM.WebStorage as Store
import Data.Functor
import Control.Applicative
import Control.Monad
import BlueWire.Crypto

getState = do
    storage <- Store.getLocalStorage
    x <- Store.getItem storage "blue-wire"
    pure 0
