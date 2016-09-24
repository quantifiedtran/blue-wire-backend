module BlueWire.Database.OrphanInstances where

import Database.Persist
import Database.Persist.Sql
import Web.Scotty
import Data.Aeson
import Data.Time.Clock

instance PersistField NominalDiffTime where
    toPersistValue = toPersistValue . toRational
    fromPersistValue = fmap fromRational . fromPersistValue

instance PersistFieldSql NominalDiffTime where
    sqlType _ = SqlReal

instance Parsable Value where
    parseParam = readEither
