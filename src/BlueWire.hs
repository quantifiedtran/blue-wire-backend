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
import Data.Aeson
import Data.Aeson.TH
import BlueWire.Types hiding (canNextSetKicks)
import BlueWire.Database.Opaleye.Schema

data PublicConfig = PublicConfig {
      timeout :: NominalDiffTime
    -- ^ The time between requests before an application is assumed to be
    -- closed.
    , thisIsABlueWireServer :: Bool
    -- ^ Boolean to reassure the client that this is the server they're looking for.
} deriving (Eq, Ord, Show)

deriveJSON defaultOptions ''PublicConfig

genInfoResponse :: Profile' -> InfoResponse
genInfoResponse app = InfoResponse (kickToUpcomingKick <$> activeKicks app) (canNextSetKicks app) where
    kickToUpcomingKick :: Kick' -> UpcomingKick
    kickToUpcomingKick kick = UpcomingKick (countdown kick) (duration kick)
