{-# LANGUAGE OverloadedStrings #-}
module BlueWire.APIGen (
      genBluewireAPIJS
    , writeBluewireAPIJS
) where

import Servant
import Servant.JS
import BlueWire.Servant
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text

{-
    General idea: use the prefix to append a reference to a this.address variable
    then preappend an object definition or a function beginning and a return in the end.
-}

preappend = "export function bluewire(config) {\nlet bw = {};"

append = "\nreturn bw;\n}"

jsgenOptions = defCommonGeneratorOptions { urlPrefix = "\' + config.address + \'", moduleName = "bw"}

{-|
    Generate the JS API for blue-wire
-}
genBluewireAPIJS :: Proxy (BlueWireAPI str config) -> Text
genBluewireAPIJS proxy = preappend <> jsForAPI proxy (vanillaJSWith jsgenOptions) <> append

{-|
    Write the JS API for blue-wire to a file.
-}
writeBluewireAPIJS :: Proxy (BlueWireAPI str config) -> FilePath -> IO ()
writeBluewireAPIJS proxy filepath = writeFile filepath (Text.unpack $ genBluewireAPIJS proxy)
