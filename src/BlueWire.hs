{-# LANGUAGE DataKinds                  #-}
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
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeOperators #-}
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
import Servant.Server
import Servant.API
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad ((<=<), (>=>))
import BlueWire.Database.Query as Query
import BlueWire.Database.Schema
import BlueWire.Types
import BlueWire.Servant
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
    -- ^ The function to run database actions with.
    , _hostname :: Text
    -- ^
}

makeLenses ''BlueWireConfig

-- bluewire :: S.ScottyM ()
bluewire (config :: BlueWireConfig) = do

    -- Heartbeat the app with the given ID, then return the action that should be taken.
    S.post "/heartbeat/:application" $ do
        -- Query the database for the app
        S.param "application" >>= runDb' . getAppWithName >>= \case
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
        -- Read the appdata from
        (app :: AppStats) <- (lastHeartbeat .~ now) <$> S.jsonData
        runDb' (getAppWithName (app^.name)) >>= \case
            Nothing -> do
                _ <- runDb' $ P.insert app
                S.json (object ["time" .= now])
            -- The application exists already.
            Just _ -> S.raise "The app profile your're trying to create already exists."

    -- Kick getter
    S.get "/get/:application/kicks" $
        S.param "application" >>= fmap (fmap P.entityVal) . runDb' . getAppWithName >>= \case
            Nothing -> noSuchApp
            Just application ->
                S.json $ object [ "kicks" .= (application^.activeKicks)
                                , "canNextSetKicks" .= nextKSTime application]

    -- Kick setter, it's use throttled to once every 2 days, might change to config-defined time period.
    S.post "/set/:application/kicks" $ do
        now <- liftIO getCurrentTime
        S.param "application" >>= runDb' . getAppWithName >>= \case
            Nothing -> S.json (Nothing :: Maybe ())
            Just application
                | canSetKicks now application -> do
                    (kicks :: [Kick]) <- S.jsonData
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

            noSuchApp = S.raise "This app profile doesn't exist"

bluewireIO port config = S.scotty port $ bluewire config

liftDb config = liftIO . (config^.runDb)

migrate :: BlueWireDB ()
migrate =
    P.runMigration migrateAll

bluewireConn :: Text -> BlueWireDB a -> IO a
bluewireConn conn = runResourceT . runNoLoggingT . P.withSqliteConn conn . P.runSqlConn

bluewireServant :: BlueWireConfig -> Server (BlueWireAPI String PublicConfig)
bluewireServant (config :: BlueWireConfig) = newProfile
                                        :<|> appAPI
                                        :<|> conf where

    conf :: ExceptT ServantErr IO PublicConfig
    conf = return (config^.publicConf)

    runDB' :: MonadIO m => BlueWireDB a -> m a
    runDB' = liftDb config

    newProfile :: AppProfile -> ExceptT ServantErr IO UTCTime
    newProfile _application = do
        now <- liftIO getCurrentTime
        let application = _application & lastHeartbeat .~ now
        result <- runDB' $ (getAppWithName $ application^.name) >>= \case
            Nothing -> do
                _ <- P.insert application
                return (Just now)
            Just _ -> return Nothing
        case result of
            Nothing -> throwE (err409 { errBody = "Profile already exists" })
            Just _ -> return now

    appAPI :: String -> Server AppAPI
    appAPI appname = getApp
                :<|> heartbeatApp
                :<|> kicksApi where

        queryAppname :: ExceptT ServantErr IO (P.Entity AppProfile)
        queryAppname = do
            maybeAppEn <- runDB' (getAppWithName appname)
            maybe (throwE err400 {errBody = "App doesn't exist"}) return maybeAppEn

        getApp :: ExceptT ServantErr IO AppProfile
        getApp = queryAppname >>= (return . P.entityVal)

        heartbeatApp :: ExceptT ServantErr IO HeartbeatResponse
        heartbeatApp = queryAppname >>= \en -> do
            -- Get the current time
            now <- liftIO getCurrentTime
            runDB' $ heartbeat (config^.publicConf.timeout) now en

        kicksApi :: Server KickAPI
        kicksApi = getKicks :<|> setKicks :<|> appendKicks where
            getKicks :: ExceptT ServantErr IO [Kick]
            getKicks = queryAppname >>= \en -> return $ (P.entityVal en)^.activeKicks

            setKicks :: ExceptT ServantErr IO (Either UTCTime InfoResponse)
            setKicks kicks = queryAppname >>= \en -> do
                now <- liftIO getCurrentTime

                runDB' $ do
                    P.update (P.entityKey en) [ActiveKicks P.=. kicks, CanNextSetKicks P.=. now]
                    P.get (P.entityKey en)

            appendKicks :: ExceptT ServantErr IO InfoResponse
            appendKicks kicks = do
                en <- queryAppname
                runDB' (P.update (P.entityKey en) [ActiveKicks P.=. (kicks ++ P.entityVal en^.activeKicks)])


            canSetKicks now application = now >= (nextKSTime . P.entityVal $ application)

            nextKSTime = \case
                AppStats { _kickEnds = Just ends, _canNextSetKicks = cnsk } ->
                    max ends cnsk
                application -> application^.canNextSetKicks
