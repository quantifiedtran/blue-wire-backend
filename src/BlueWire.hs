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

import Data.Time
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.TH
import Data.Proxy
import Data.Text (Text)
import Data.Maybe
import Servant.Server
import Servant.API
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import qualified Network.Wai.Handler.Warp as Warp
import Control.Monad ((<=<), (>=>))
import BlueWire.Database.Query as Query
import BlueWire.Database.Schema
import BlueWire.Types hiding (canNextSetKicks)
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
}

makeLenses ''BlueWireConfig

bluewireAPI :: Proxy (BlueWireAPI String PublicConfig)
bluewireAPI = Proxy

bluewireIO :: Int -> BlueWireConfig -> IO ()
bluewireIO port config = Warp.run port $ serve bluewireAPI (bluewireServant config)

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
        getApp = P.entityVal <$> queryAppname

        heartbeatApp :: ExceptT ServantErr IO HeartbeatResponse
        heartbeatApp = queryAppname >>= \en -> do
            -- Get the current time
            now <- liftIO getCurrentTime
            runDB' $ heartbeat (config^.publicConf.timeout) now en

        kicksApi :: Server KickAPI
        kicksApi = getKicks :<|> setKicks :<|> appendKicks where
            getKicks :: ExceptT ServantErr IO [Kick]
            getKicks = queryAppname >>= \en -> return $ P.entityVal en^.activeKicks

            setKicks :: [Kick] -> ExceptT ServantErr IO (Either UTCTime InfoResponse)
            setKicks kicks = queryAppname >>= \en -> do
                now <- liftIO getCurrentTime
                if canSetKicks now en then
                    maybe (throwE err400) (return . Right . genInfoResponse) <=< runDB' $ do
                        P.update (P.entityKey en) [ActiveKicks P.=. kicks, CanNextSetKicks P.=. now]
                        P.get (P.entityKey en)
                    else return . Left $ P.entityVal en^.canNextSetKicks

            {-|
                Endpoint that appends new kicks to the app profile's active kicks.
            -}
            appendKicks :: [Kick] -> ExceptT ServantErr IO InfoResponse
            appendKicks kicks = do
                en <- queryAppname
                maybe (throwE err400) (return . genInfoResponse) <=< runDB' $ do
                    P.update (P.entityKey en) [ActiveKicks P.=. (kicks ++ P.entityVal en^.activeKicks)]
                    P.get (P.entityKey en)

            canSetKicks now application = now >= (nextKSTime . P.entityVal $ application)

            nextKSTime = \case
                AppStats { _kickEnds = Just ends, _canNextSetKicks = cnsk } ->
                    max ends cnsk
                application -> application^.canNextSetKicks

genInfoResponse :: AppProfile -> InfoResponse
genInfoResponse app = InfoResponse (kickToUpcomingKick <$> app^.activeKicks) (app^.canNextSetKicks) where
    kickToUpcomingKick :: Kick -> UpcomingKick
    kickToUpcomingKick kick = UpcomingKick (kick^.countdown) (kick^.duration)
