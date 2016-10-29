{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE LambdaCase                 #-}
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
    {- screaming -}
    let
        -- The difference in time between now and the last heartbeat.
        diff :: NominalDiffTime
        diff = diffUTCTime now (appstat^.lastHeartbeat)

        -- The time recovered by
        recoveredTime :: NominalDiffTime
        recoveredTime
            | diff < appstat^.recoveryHurdle = 0
            | otherwise = min (appstat^.maxRecovery) $ (diff - appstat^.recoveryHurdle) * appstat^.recoveryRate

    in case appstat of
        -- There's a kick active
        AppStats { _kickEnds = Just ends, _activeKicks = _kicks }
            -- return the date and time the kick ends without an updated list of kicks
            -- unless the time the kick ends has passed, then just return the list of active kicks.
            | ends > now -> Left (ends, Nothing)
            | otherwise -> Right _kicks

        -- No active kick
        AppStats { _activeKicks = _kicks }
            -- If a time greater than the timeout has passed, add recovered time to the kicks
            | diff >= timeout -> Right (_kicks <&> countdown +~ recoveredTime)
            -- The heartbeat is within the timeout
            | otherwise ->
                let
                    -- Subtract the difference in time from all the kick countdowns
                    kicks :: [Kick]
                    kicks = _kicks <&> countdown -~ diff

                    -- Get all the kicks whose countdowns have reached 0 and those who haven't
                    (triggeredKicks, untriggeredKicks) = partition ( (<= 0) . _countdown ) kicks

                    -- Triggered kicks updated, with non-repeating kicks removed.
                    postTriggeredKicks :: [Kick]
                    postTriggeredKicks = do
                        kick <- triggeredKicks
                        case kick^.repeatCountdown of
                            Nothing -> []
                            Just cd -> return $ kick & countdown .~ cd

                    -- The time the kick ends, if triggered. This value is 'now' if no kicks triggered.
                    kickEndsAt :: UTCTime
                    kickEndsAt = addUTCTime (sum $ _duration <$> triggeredKicks) now

                in  if null triggeredKicks then Right kicks -- If no kicks were triggered, just return the updated list of kicks
                    else
                        -- Some kicks were triggered, return the updated list of kicks and the
                        -- time the kick ends.
                        Left (kickEndsAt, Just (postTriggeredKicks ++ untriggeredKicks))
