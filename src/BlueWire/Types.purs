module BlueWire.Types where

import Prelude
import Data.Set
import Data.Time
import Data.Boolean
import Data.Generic
import Data.Maybe
import Data.Time.Duration
import Data.DateTime (DateTime(DateTime))
import Data.Array
import Data.Map (Map)

-- | blue-wire config datatype.
type BlueWireConf = {
      directory :: String
    , messages :: Set BlueWireMessage
}

-- | a `blue-wire` message.
type BlueWireMessage = {
      times :: Set { hour :: Hour, minute :: Minute }
    , reminderText :: String
}

type BlueWireRequest dur = {
      timePassed :: dur
}

-- | The stored state of blue-wire, containing the kicks
newtype BlueWireState dur = BlueWireState {
      kickQueue :: Array (Kick dur)
    , blacklistedTimes :: Set (Time)
    , currentKickEnds :: Maybe (DateTime)
    , lastOnline :: DateTime
    , regainRate :: Number
    , minimalTimeBeforeAnyRegain :: dur
}

derive instance genericMap :: (Generic k, Generic v) => Generic (Map k v)
derive instance genericSet :: Generic a => Generic (Set a)
derive instance genericBlueWireState :: Generic dur => Generic (BlueWireState dur)

-- | A kick, containing the kick duration and the time until the next kick.
-- | Additionally,
type Kick dur = {
      duration :: dur
    , timeUntil :: dur
}
