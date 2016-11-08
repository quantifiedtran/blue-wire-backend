{-# LANGUAGE OverloadedStrings #-}
module Main where

import BlueWire
import Options
import Data.Text (pack)
import qualified Database.Persist.Sqlite as P
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.IO.Class

data BWOpts = BWOpts { port :: Int, dbfile :: String, poolSize :: Int }

instance Options BWOpts where
    defineOptions = BWOpts <$> simpleOption "port" 8080 "The port to run the server on"
                           <*> simpleOption "db" "blue-wire.db" "The database to use"
                           <*> simpleOption "pooln" 10 "The size of the connection pool"

main :: IO ()
main =
    runCommand $ \opts _ ->
        runNoLoggingT . P.withSqlitePool (pack $ dbfile opts) (poolSize opts) $ \pool -> do
            let runDB = bluewireConnWithPool pool
            runDB migrate
            liftIO $ bluewireIO (port opts) (BlueWireConfig (PublicConfig 30 True) (liftIO . runNoLoggingT . runDB))
