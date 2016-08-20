module BlueWire.Types where

import Prelude
import Data.Set
import Data.Time
import Data.Boolean
import Data.Generic
import Data.Maybe
import Data.Time.Duration
import Data.DateTime (DateTime(DateTime))

-- | blue-wire config datatype.
data BlueWireConf = BlueWireConf {
      directory :: String
    , messages :: Set BlueWireMessage
}

-- | a `blue-wire` message.
data BlueWireMessage = BlueWireMessage {
      times :: Set { hour :: Hour, minute :: Minute }
    , reminderText :: String
}

data BlueWireRequest dur = BlueWireRequest {
      timePassed :: dur
}

-- | The stored state of blue-wire, containing the kicks
data BlueWireState queue dur = BlueWireState {
      kickQueue :: queue (Kick dur)
    , currentKickEnds :: Maybe (DateTime)
}

-- | A kick, containing the kick duration and the time until the next kick.
-- | The timeUntilDuration kick
data Kick dur = Kick {
      kickDuration :: dur
    , timeUntilKick :: dur
    , borrowedTime :: dur

}

derive instance kickGeneric :: Generic dur => Generic (Kick dur)
