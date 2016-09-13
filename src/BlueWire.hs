{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
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

-- bluewire :: S.ScottyM ()
bluewire (config :: PublicConfig) dbRun = do
    S.get "/heartbeat/:application" $ return ()
    S.get "/exists/:application" $ return ()
    S.get "/create-app-profile/:application/:kick-config" $ do
        return ()
    S.get "/config" $ do
        S.json config

-- bluewireIO :: Int -> PublicConfig -> IO ()
bluewireIO port config dbRun = S.scotty port $ bluewire config dbRun

bluewireIO' port config = bluewireIO port config defaultDbRun

defaultDbRun action = runResourceT (P.runSqlite ":memory:" action)
