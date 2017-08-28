{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Brick as B
import qualified Brick.Widgets.List as B
import qualified Servant.Client as S
import BlueWire.Types
import BlueWire.Servant
import BlueWire.Database.Opaleye.Schema (Profile')
import qualified Data.Aeson as JSON
import qualified Graphics.Vty.Attributes as B (defAttr)

main :: IO ()
main = putStrLn "nope"

-- | The views in the TUI
data ViewName
    = Menu
    | Server
    | LocalConfig
    deriving (Eq, Ord, Show)

type BWEvent = ()

-- | the configuration of the program
data ViewConfig = ViewConfig {
    shouldAutoloadServer :: Bool,
    autoloadServer :: Maybe (String, Maybe String)
} deriving (Eq, Ord, Show)

defaultVC = ViewConfig [] False Nothing

data BWVSum
    = ServerView
        { currentServer :: Maybe (String, Maybe Profile')
        , knownServers :: [(String, [String])]
        }
    | ConfigView { bwvConfig :: ViewConfig }
    | MenuView
    deriving (Eq, Ord, Show)

data BWVState = BWVState
    { view :: BWVSum
    , confFile :: String
    }

renderState :: BWVState -> [B.Widget ViewName]
renderState BWVState {..} = [wig]
    where
        wig = case view of
                Menu ->
                    let title = "Menu"
                        list = []
                    in undefined
                Server -> undefined
                LocalConfig -> undefined

menuList =
    let list = []
    in B.list

handleEvents :: BWVState
             -> B.BrickEvent ViewName BWEvent
             -> B.EventM ViewName (B.Next BWVState)
handleEvents BWVS{..} ev =
    case currentNav of
        MenuView -> case ev of
            B.VtyEvent vtyev -> B.handleListEvent vtyev
            _ -> undefined

bwapp = B.App
    { appDraw = renderState
    , appChooseCursor = B.neverShowCursor
    , appStartEvent = return
    , appAttrMap = const $ B.attrMap B.defAttr []
    , appHandleEvent = handleEvents
    }
