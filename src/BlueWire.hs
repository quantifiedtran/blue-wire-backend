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
import BlueWire.Types
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
    , _runDb :: forall a. BlueWireDB a -> IO a
}

makeLenses ''BlueWireConfig

-- bluewire :: S.ScottyM ()
bluewire (config :: BlueWireConfig) = do

    -- Heartbeat the app with the given ID, then return the action that should be taken.
    S.get "/heartbeat/:application" $ do
        -- Query the database for the app
        maybeApp <- liftDb config . getAppWithName =<< S.param "application"
        -- pattern match on the maybe type for two branches in actions
        case maybeApp of
            -- The app doesn't exist, return `Nothing`
            Nothing -> S.json (Nothing :: Maybe ())
            -- The app exists! run the heartbeat on the app and return
            -- relevant information about the app to the request.
            Just application -> do
                -- Get the current time in UTC
                now <- liftIO getCurrentTime
                liftDb config (heartbeat (config^.publicConf.timeout) now application) >>= S.json

    -- Returns the application ID once finished.
    S.get "/create-app-profile/:application/:initial-kick" $ do
        now <- liftIO getCurrentTime

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
