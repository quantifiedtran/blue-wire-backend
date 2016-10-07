{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE LambdaCase                 #-}
module BlueWire where

-- got a lot of help from looking at this gist:
-- https://gist.github.com/egonSchiele/5400694#file-main-hs-L50

import qualified Web.Scotty as S
import Data.Time
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.Maybe
import Control.Monad.IO.Class
import BlueWire.Database.Query
import BlueWire.Database.Schema
import BlueWire.Types
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import qualified Database.Persist.Sqlite as P
import Control.Monad.Trans.Resource
import Control.Monad.Logger

data PublicConfig = PublicConfig {
      _timeout :: NominalDiffTime
    -- ^ The time between requests before an application is assumed to be
    -- closed.
    , _thisIsABlueWireServer :: Bool
    -- ^ Boolean to reassure the client that this is the server they're looking for.
} deriving (Eq, Ord, Show)

makeLenses ''PublicConfig
deriveJSON (defaultOptions {fieldLabelModifier = dropWhile (== '_') }) ''PublicConfig

data BlueWireConfig = BlueWireConfig {
      _publicConf :: PublicConfig
    -- ^ The publicly available configuration
    , _runDb :: forall a. BlueWireDB a -> IO a
}

makeLenses ''BlueWireConfig

-- bluewire :: S.ScottyM ()
bluewire (config :: BlueWireConfig) = do

    -- Heartbeat the app with the given ID, then return the action that should be taken.
    S.post "/heartbeat/:application" $ do
        -- Query the database for the app
        maybeApp <- runDb' . getAppWithName =<< S.param "application"
        -- pattern match on the maybe type for two branches in actions
        case maybeApp of
            -- The app doesn't exist, return `Nothing`
            Nothing -> S.json (Nothing :: Maybe ())
            -- The app exists! run the heartbeat on the app and return
            -- relevant information about the app to the request.
            Just application -> do
                -- Get the current time in UTC
                now <- liftIO getCurrentTime
                runDb' (heartbeat (config^.publicConf.timeout) now application) >>= S.json

    -- Returns the application ID once finished.
    S.post "/new" $ do
        -- Get the current time
        now <- liftIO getCurrentTime
        --
        (app :: AppStats) <- (lastHeartbeat .~ now) <$> S.jsonData
        _ <- runDb' $ P.insert app
        S.json (object ["time" .= now])

    -- Kick getter
    S.get "/get/:application/kicks" $
        S.param "application" >>= fmap (fmap P.entityVal) . runDb' . getAppWithName >>= \case
            Nothing -> S.json (Nothing :: Maybe ())
            Just application ->
                S.json $ object [ "kicks" .= (application^.activeKicks)
                                , "canNextSetKicks" .= nextKSTime application]

    -- Kick setter, it's use throttled to once every 2 days, might change to config-defined time period.
    S.post "/set/:application/kicks/:kicks" $ do
        now <- liftIO getCurrentTime
        S.param "application" >>= runDb' . getAppWithName >>= \case
            Nothing -> S.json (Nothing :: Maybe ())
            Just application
                | canSetKicks now application ->
                    fromJSON <$> S.param "kicks" >>= \case
                        Error err -> S.json $ object ["error" .= err]
                        Success (kicks :: [Kick]) -> do
                            let cnsk = addUTCTime (60 * 60 * 24 * 2 {- 2 days -}) now
                            runDb' $ P.update (P.entityKey application) [ ActiveKicks P.=. kicks
                                                                        , CanNextSetKicks P.=. cnsk]
                            S.json $ object ["kicks" .= kicks, "canNextSetKicks" .= cnsk]

                | otherwise -> S.json (object ["canNextSetKicks" .= nextKSTime (P.entityVal application)])

    -- get the publicly available config.
    S.get "/config" $ do
        S.json (config^.publicConf) where
            runDb' :: MonadIO m => BlueWireDB a -> m a
            runDb' = liftDb config

            canSetKicks now application = now >= (nextKSTime . P.entityVal $ application)

            nextKSTime = \case
                AppStats { _kickEnds = Just ends, _canNextSetKicks = cnsk } ->
                    max ends cnsk
                application -> application^.canNextSetKicks

bluewireIO port config = S.scotty port $ bluewire config

liftDb config = liftIO . (config^.runDb)

migrate :: BlueWireDB ()
migrate =
    P.runMigration migrateAll

bluewireConn :: Text -> BlueWireDB a -> IO a
bluewireConn conn = runResourceT . runNoLoggingT . P.withSqliteConn conn . P.runSqlConn
