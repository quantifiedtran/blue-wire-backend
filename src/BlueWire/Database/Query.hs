{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module BlueWire.Database.Query where

import BlueWire.Database.Schema
import Database.Persist
import Database.Persist.Class
import Database.Persist.TH
import Data.Maybe
import Control.Monad.IO.Class

{-|
    Poll if a app with a given name exists.
-}
exists name = do
    app <- getAppWithName name
    return $ isJust app

{-|
    Get an app if it exists.
-}
getAppWithName name = do
    list <- selectList [AppStatsName ==. name] [LimitTo 1]
    case list of
        [] -> return Nothing
        (x:_) -> return $ Just x

createAppProfile name kick = undefined
