{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
module BlueWire.Database.Schema where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Data.Aeson.TH
import Data.Time
import Data.Typeable
import Data.Generics
import Control.Monad.Trans.Resource

import BlueWire.Types
import BlueWire.Database.OrphanInstances()

share [mkPersist sqlSettings { mpsGenerateLenses = True, mpsPrefixFields = False }
      , mkMigrate "migrateAll"] [persistLowerCase|
Kick
    duration NominalDiffTime       -- ^ The duration of the kick, in seconds.
    countdown NominalDiffTime      -- ^ The total time before, an actual mutable value.
    repeatCountdown NominalDiffTime Maybe -- ^ Should this kick repeat when the kick time is over and if so, use this value (null if no repeat).
    deriving Show Data Typeable

-- the new Kick representation
KickNew
    knDuration NominalDiffTime
    knCountdown NominalDiffTime
    knRepeatCountdown NominalDiffTime Maybe
    knRecoveryHurdle NominalDiffTime
    knRecoveryRate NominalDiffTime

AppStats
    name String              -- ^ The name of the application
    UniqueAppStatsName name  -- no two appstats can have the same app name.
    activeKicks [Kick]
    lastHeartbeat UTCTime    -- ^ The last time a heartbeat was recieved
    kickEnds UTCTime Maybe   -- ^ The time when the active kick will end, Nothing if there's no active kick.
    recoveryRate NominalDiffTime   -- ^ Rate of recovery, should be >= 0
    recoveryHurdle NominalDiffTime -- ^ The minimum amount of time that should have passed before any recovery time is counted.
    maxRecovery NominalDiffTime    -- ^ The maximum amount of recovery time for a given kick.
    canNextSetKicks UTCTime  -- ^ The next time the kicks can be set.
    deriving Show Data Typeable
|]

deriveJSON (defaultOptions {fieldLabelModifier = dropWhile (== '_') }) ''Kick
deriveJSON (defaultOptions {fieldLabelModifier = dropWhile (== '_') }) ''AppStats

{-|
    Better named alias.
-}
type AppProfile = AppStats
