module Main where

import BlueWire

main :: IO ()
main = bluewireIO 8080 (BlueWireConfig (PublicConfig 30) undefined)
