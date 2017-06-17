{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module BlueWire.Logic where

import BlueWire.Database.Opaleye.Schema
import Control.Monad.State
import BlueWire.Types
import Data.Time.Clock

keepalive :: MonadState Profile' m => UTCTime -> m KeepaliveResponse
keepalive = undefined

-- | Calculate the recovery time for a countdown based on a `Recovery` profile.
recover :: Recovery'       -- ^ The recovery profile
        -> NominalDiffTime -- ^ The time passed
        -> NominalDiffTime -- ^ The time left in a countdown
        -> NominalDiffTime -- ^ The time once recovered
recover rp passed time
    -- The time passed is less than the hurdle, so return the time as is
    | passed <= hurdle rp = time
    -- The hurdle has been passed.
    | otherwise =
        let --
            recovered = (passed - hurdle rp) * rate rp
        in min (maxTime rp) (recovered + time)

recoverOnKick :: NominalDiffTime -> Kick' -> Kick'
recoverOnKick passed kick@Kick { recoveryProfile = Just rp }
    = kick { countdown = recover rp passed $ countdown kick }
recoverOnKick _      kick = kick
