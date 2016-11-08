module BlueWire.Database.Query where

import Database.Persist
import Database.Persist.Class
import Database.Persist.TH
import Data.Maybe
import Control.Monad.IO.Class
import Control.Lens
import Data.Time
import Data.Bifunctor
import Data.Function
import Data.Either
import Data.List

import BlueWire.Database.Schema
import BlueWire.Types hiding (canNextSetKicks)

{-|
    Get an app if it exists.
-}
getAppWithName :: String -> BlueWireDB (Maybe (Entity AppStats))
getAppWithName name = do
    list <- selectList [Name ==. name] [LimitTo 1]
    case list of
        [] -> return Nothing
        (x:_) -> return $ Just x

withAppNamed :: String -> BlueWireDB a -> (Entity AppProfile -> BlueWireDB a) -> BlueWireDB a
withAppNamed appname def action = getAppWithName appname >>= maybe def action

withAction :: Monad m => (k -> m (Maybe b)) -> k -> m c -> (b -> m c) -> m c
withAction lookukValue key def action =
    lookukValue key >>= maybe def action

{-|
    Update the time of the last heartbeat for a given app, and if
    a kick is made active by the difference in time between now and
    the previous heartbeat then activate that kick
-}
heartbeat :: NominalDiffTime
          -> UTCTime
          -> Entity AppStats
          -> BlueWireDB HeartbeatResponse
heartbeat timeout now applicationEntity = do
        update (entityKey applicationEntity) updates
        return hbresponse where

            application :: AppStats
            application = entityVal applicationEntity

            -- The updates to the database entry.
            updates :: [Update AppStats]
            updates = [LastHeartbeat =. now, ActiveKicks =. kicks, KickEnds =. kicktime]

            kicktime :: Maybe UTCTime
            kicktime = either (Just . fst) (const Nothing) hbpure

            -- The kick result from the heartbeat
            kicks :: [Kick]
            kicks = either (fromMaybe (application^.activeKicks) . snd) id hbpure

            -- The result from the pure heartbeat logic
            hbpure :: Either (UTCTime, Maybe [Kick]) [Kick]
            hbpure = heartbeatLogic timeout now application

            -- The response to give from the heartbeat
            hbresponse :: HeartbeatResponse
            hbresponse = bimap (KickResponse . fst) (\x -> InfoResponse (fmap upcoming x) (application^.canNextSetKicks)) hbpure

            -- pull out info from a Kick to am UpcomingKick
            upcoming :: Kick -> UpcomingKick
            upcoming kick = UpcomingKick (kick^.countdown) (kick^.duration)

{-|
    Logic for heartbeat requests, in a pure function
-}
heartbeatLogic :: NominalDiffTime
               -- ^ The maximum time between heartbeats
               -> UTCTime
               -- ^ The current time
               -> AppStats
               -- ^ The app profile we're using
               -> Either (UTCTime, Maybe [Kick]) [Kick]
               -- ^ Either the time the current kick ends and a possible list of
               -- kicks if some have been removed, or a list of kicks with their
               -- countdowns updated.
heartbeatLogic timeout now appstat =
    let
        -- The difference in time between now and the last heartbeat.
        diff :: NominalDiffTime
        diff = diffUTCTime now (appstat^.lastHeartbeat)

        -- Update the kicks and partition them into activated and
        updateKicks :: NominalDiffTime -> [Kick] -> ([Kick], [Kick])
        updateKicks passed kicks = normalKicks $ kicks <&> countdown -~ passed

        -- Normalise kicks after they've had time subtracted, so kicks don't fall out of
        -- sync with eachother.
        -- Most time past kick time is priority.
        normalKicks :: [Kick] -> ([Kick], [Kick])
        normalKicks kicks =
            let (activated, counting) = partition ((<= 0) . _countdown) kicks
                overtime = abs . fromMaybe 0 . minimumOf traverse $ _countdown <$> activated
            in (activated <&> countdown +~ overtime, counting <&> countdown +~ overtime)

        -- Given a list of kicks that have been activated, reset any kicks with a repeating
        -- countdown and discard any that don't.
        processActivated :: [Kick] -> [Kick]
        processActivated kicks = do
            -- for each kick
            kick <- kicks
            -- check if it has a repeating countdown
            case kick^.repeatCountdown of
                -- if it doesn't, then it gets filtered out with concatMap
                Nothing -> []
                -- otherwise, then reset it's countdown to the repeating value.
                Just repeating -> return (kick & countdown .~ repeating)

    in case appstat of
        -- There's a kick active
        AppStats { _kickEnds = Just ends, _activeKicks = _kicks }
            -- return the date and time the kick ends without an updated list of kicks
            -- unless the time the kick ends has passed, then just return the list of active kicks.
            | ends >= now -> Left (ends, Nothing)
            | otherwise -> Right _kicks

        -- No active kick
        AppStats { _activeKicks = _kicks }
            -- If a time greater than the timeout has passed, add recovered time to the kicks
            | diff >= timeout -> Right (recoverOnKick (diff - timeout) <$> _kicks)
            -- The heartbeat is within the timeout
            | otherwise ->
                let
                    -- Update and normalise the kicks, also split them between
                    (activated, countingDown) = updateKicks diff _kicks

                    -- The time the kick ends, if triggered. This value is 'now' if no kicks triggered.
                    kickEndsAt :: UTCTime
                    kickEndsAt = addUTCTime (sum (_duration <$> activated)) now

                in  if null activated then Right countingDown -- If no kicks were triggered, just return the updated list of kicks
                    else
                        -- Some kicks were triggered, return the updated list of kicks and the
                        -- time the kick ends.
                        Left (kickEndsAt, Just (processActivated activated ++ countingDown))

-- | Calculate the recovery time for a countdown based on a `Recovery` profile.
recover :: Recovery        -- ^ The recovery profile
        -> NominalDiffTime -- ^ The time passed
        -> NominalDiffTime -- ^ The time left in a countdown
        -> NominalDiffTime -- ^ The time once recovered
recover rp passed time
    -- The time passed is less than the hurdle, so return the time as is
    | passed <= rp^.hurdle = time
    -- The hurdle has been passed.
    | otherwise =
        let --
            recovered = (passed - rp^.hurdle) * rp^.rate
        in min (rp^.maxTime) (recovered + time)

recoverOnKick :: NominalDiffTime -> Kick -> Kick
recoverOnKick passed kick@Kick { _recoveryProfile = Just rp } = kick & countdown %~ recover rp passed
recoverOnKick _      kick = kick
