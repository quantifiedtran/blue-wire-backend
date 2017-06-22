{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module BlueWire.Logic where

import BlueWire.Database.Opaleye.Schema
import Control.Monad.State
import Control.Arrow
import BlueWire.Types
import Data.Time.Clock
import Opaleye hiding (min)

keepalive :: MonadState Profile' m => UTCTime -> m KeepaliveResponse
keepalive timeNow = undefined

-- keepaliveOpal :: Table ProfileCol ProfileCol 

-- | Calculate the recovery time for a countdown based on a `Recovery` profile.
recover :: Recovery'       -- ^ The recovery profile
        -> NominalDiffTime -- ^ The time passed
        -> NominalDiffTime -- ^ The time left in a countdown
        -> NominalDiffTime -- ^ The time once recovered
recover Recovery{..} passed time
    -- The time passed is less than the hurdle, so return the time as is
    | passed <= hurdle = time
    -- The hurdle has been passed.
    | otherwise =
        let --
            recovered = (passed - hurdle) * rate
        in min maxTime (recovered + time)

recoverOnKick :: NominalDiffTime -> Kick' -> Kick'
recoverOnKick passed kick@Kick { recoveryProfile = Just rp, .. }
    = kick { countdown = recover rp passed countdown }
recoverOnKick _ kick = kick
