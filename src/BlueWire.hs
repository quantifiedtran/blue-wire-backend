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
module BlueWire where

import qualified Web.Scotty as S
import Data.Time
import Control.Lens
import Data.Aeson.TH
import Control.Monad.IO.Class
import BlueWire.Database.Query
import BlueWire.Database.Schema
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import qualified Database.Persist.Sqlite as P
import Control.Monad.Trans.Resource


data PublicConfig = PublicConfig {
      _timeout :: NominalDiffTime
    -- ^ The time between requests before an application is assumed to be
    -- closed.
} deriving (Eq, Ord, Show)

makeLenses ''PublicConfig
deriveJSON (defaultOptions {fieldLabelModifier = dropWhile (== '_') }) ''PublicConfig

data BlueWireConfig = BlueWireConfig {
      _publicConf :: PublicConfig
    -- ^ The publicly available configuration
    , _runDb :: forall a. P.SqlPersist (ResourceT IO) a -> IO a
}

makeLenses ''BlueWireConfig

-- bluewire :: S.ScottyM ()
bluewire (config :: BlueWireConfig) = do

    -- Heartbeat the app with the given ID.
    S.get "/heartbeat/:application" $ do
        application <- liftDb config . getAppWithName =<< S.param "application"
        case P.entityKey <$> application of
            Nothing -> S.json (Nothing :: Maybe ())
            Just appId -> do
                now <- liftIO getCurrentTime
                liftDb config (heartbeat now appId) >>= S.json

    -- Returns the application ID once finished.
    S.get "/create-app-profile/:application/:kick-config" $ do
        return ()

    S.get "/kicks-of/:application" $ do
        return ()

    -- get the publicly available config.
    S.get "/config" $ do
        S.json (config^.publicConf)

-- bluewireIO :: Int -> PublicConfig -> IO ()
bluewireIO port config = S.scotty port $ bluewire config

-- bluewireIO' port config = bluewireIO port config defaultDbRun

-- defaultDbRun :: MonadIO m => P.SqlPersistT backend a -> m a
-- defaultDbRun action = runResourceT (P.runSqlite ":memory:" action)

-- x = P.withSqlConn "dev.sqlite3"

liftDb config = liftIO . (config^.runDb)
