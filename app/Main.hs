{-# LANGUAGE OverloadedStrings #-}
module Main where

import BlueWire
import Options
import Data.Text (pack)

data BWOpts = BWOpts { port :: Int, dbfile :: String }

instance Options BWOpts where
    defineOptions = BWOpts <$> simpleOption "port" 8080 "The port to run the server on"
                           <*> simpleOption "db" "blue-wire.db" "The database to use"

main :: IO ()
main = runCommand $ \opts _ -> do
    let runDB = bluewireConn . pack . dbfile $ opts
    runDB migrate
    bluewireIO (port opts) (BlueWireConfig (PublicConfig 30) runDB)
