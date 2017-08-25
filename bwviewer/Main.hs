{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Brick as B
import qualified Servant.Client as S
import BlueWire.Types
import BlueWire.Servant
import BlueWire.Database.Opaleye.Schema (Profile')
import qualified Data.Aeson as JSON
import qualified Graphics.Vty.Attributes as B (defAttr)

main :: IO ()
main = putStrLn "nope"

-- | The views in the TUI
type ViewName
    = ()

data Nav
    = Menu
    | Server
    | LocalConfig
    deriving (Eq, Ord, Show)

type BWEvent = ()

-- | the configuration of the program
data ViewConfig = ViewConfig {
    knownServers :: [String],
    shouldAutoloadServer :: Bool,
    autoloadServer :: Maybe (String, Maybe String)
} deriving (Eq, Ord, Show)

defaultVC = ViewConfig [] False Nothing

data BWVState = BVWS {
    currentServerView :: Maybe (String, Maybe Profile'),
    configFilename :: String,
    currentNav :: Nav,
    viewConfig :: ViewConfig
} deriving (Eq, Ord, Show)

renderState :: BWVState -> [B.Widget ViewName]
renderState BVWS{..} = [wig]
    where
        wig = case currentNav of
                Menu ->
                    let title = "Menu"
                        list = [ ("Connect to server", Server)
                               , ("View & edit config", LocalConfig)
                               ]
                    in undefined
                Server -> undefined
                LocalConfig -> undefined

handleEvents :: BWVState
             -> B.BrickEvent ViewName BWEvent
             -> B.EventM ViewName (B.Next BWVState)
handleEvents state = \case
    _ -> undefined

bwapp = B.App
    { appDraw = renderState
    , appChooseCursor = B.neverShowCursor
    , appStartEvent = return
    , appAttrMap = const $ B.attrMap B.defAttr []
    , appHandleEvent = handleEvents
    }
