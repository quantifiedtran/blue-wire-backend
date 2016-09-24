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

-- got a lot of help from looking at this gist:
-- https://gist.github.com/egonSchiele/5400694#file-main-hs-L50

import qualified Web.Scotty as S
import Data.Time
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens (key)
import Data.Aeson.TH
import Data.Text (Text)
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
    S.post "/create-app-profile/:application-json" $ do
        -- Get the current time
        now <- liftIO getCurrentTime
        --
        (_app :: Result AppStats) <- fmap (lastHeartbeat .~ now) . fromJSON <$> S.param "application-json"
        case _app of
            Error err -> S.json (object ["error" .= err])
            Success app -> do
                -- insert the application into the database
                _ <- liftDb config $ P.insert app
                S.json (object ["time" .= now])


    S.get "/kicks-of/:application" $ do
        return ()

    -- get the publicly available config.
    S.get "/config" $ do
        S.json (config^.publicConf)

bluewireIO port config = S.scotty port $ bluewire config

liftDb config = liftIO . (config^.runDb)

migrate :: BlueWireDB ()
migrate = do
    P.runMigration migrateAll

bluewireConn :: Text -> BlueWireDB a -> IO a
bluewireConn conn = runResourceT . runNoLoggingT . P.withSqliteConn conn . P.runSqlConn
