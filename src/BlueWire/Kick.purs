module BlueWire.Kick where

import Data.DateTime (DateTime, diff, adjust)
import Data.Ring (class Ring, zero, (-), (*))
import Data.Maybe (Maybe)
import Data.Ord (class Ord, (<), (<=))
import BlueWire.Types (Kick)
import Data.Either (Either(..))
import Data.Time.Duration (class Duration)
import Prelude (otherwise)

-- | Given the time passed (first arg) and the latest
-- | countdown before shutting down the app (second arg),
-- | either give a function that will set the time when the
-- | kick is over or give a kick with it's `timeUntil` method
-- | reduced by the time passed.
kickCountdown :: forall dur. (Ring dur, Ord dur, Duration dur)
              => dur
              -> Kick dur
              -> Either (DateTime -> Maybe DateTime) (Kick dur)
kickCountdown dur kick
    | (kick.timeUntil - dur) <= zero = Left (adjust kick.duration)
    | otherwise                      = Right kick { timeUntil = kick.timeUntil - dur }

-- | Calculates extra time added to the countdown based on the user taking a break
-- | on their own.
-- |
-- | `regain.rate`: how much extra time is gained for time the program being watched is closed.
-- |
-- | `regain.hurdle`: the minimum time that has to pass before any extra time is given.
-- |
-- | `DateTime` argument 1: the date and time that the program was last closed
-- |
-- | `DateTime` argument 2: the date and time that the program was opened
-- |
-- | Result: the extra working time gained from the break.
breakRegain :: forall regain dur. (Ring dur, Ord dur, Duration dur)
            => { rate :: dur
               , hurdle :: dur
               | regain }
            -> DateTime
            -> DateTime
            -> dur
breakRegain regain exited opened
    | diff exited opened < regain.hurdle = zero
    | otherwise                          = regain.rate * (diff exited opened - regain.hurdle)
