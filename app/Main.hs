module Main where

import BlueWire

main :: IO ()
main = bluewireIO 8080 (BWBConfig 30)
