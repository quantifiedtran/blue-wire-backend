{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BlueWire.Types where

import Database.Persist.Sql
import Control.Monad.Trans.Resource
import Data.Time
import Data.Aeson
import Data.Aeson.TH

{-|
    Type alias for the Persist monad being used.
-}
type BlueWireDB = SqlPersistT (ResourceT IO)

{-|
    The response from heartbeat request.
-}
type HeartbeatResponse = Either KickResponse InfoResponse

{-|
    The response when the heartbeat check results in telling the application that it should close
-}
data KickResponse = KickResponse {
      kickEndsOn :: UTCTime
     -- ^ The time when the kick will end.
} deriving (Eq, Ord, Show)

{-|
    Information on an upcoming kick.
-}
data UpcomingKick = UpcomingKick {
      timeUntilKick :: NominalDiffTime
    -- ^ The time, if the application were to remain open, until the application would be
    --   told to close.
    , kickWouldLast :: NominalDiffTime
    -- ^ The time that the kick will last.
} deriving (Eq, Ord, Show)

{-|
    The response when the heartbeat check just registers a passing of time.
-}
data InfoResponse = InfoResponse {
      upcomingKicks :: [UpcomingKick]
      -- ^ A list of the upcoming kicks.
} deriving (Eq, Ord, Show)

deriveJSON defaultOptions ''KickResponse
deriveJSON defaultOptions ''UpcomingKick
deriveJSON defaultOptions ''InfoResponse