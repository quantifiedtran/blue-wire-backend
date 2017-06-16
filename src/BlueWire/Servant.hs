{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-|
    Module for blue-wire API expressed in Servant.
-}
module BlueWire.Servant where

import Servant.API
import BlueWire.Types
import BlueWire.Database.Opaleye.Schema
import Data.Time
import Data.Proxy

-- | The api for looking at and modifying `Profile'`s in the database.
type AppAPI =                 Get  '[JSON] Profile'
          :<|> "heartbeat" :> Post '[JSON] HeartbeatResponse
          :<|> "kicks"     :> KickAPI

-- | The api for looking at and modifying `Kick`s specifically.
type KickAPI =                                    Get  '[JSON] [Kick]
          :<|>          ReqBody '[JSON] [Kick] :> Post '[JSON] (Either UTCTime InfoResponse)
          :<|> "add" :> ReqBody '[JSON] [Kick] :> Post '[JSON] InfoResponse

{-|
    The API type for blue-wire.
-}
type BlueWireAPI str config
      =  "new"       :> ReqBody '[JSON] Profile' :> Post '[JSON] UTCTime
    :<|> "app"       :> Capture "application" str  :> AppAPI
    :<|> "config"                                  :> Get  '[JSON] config

type BlueWireServer str config = "dashboard" :> Raw :<|> BlueWireAPI str config
