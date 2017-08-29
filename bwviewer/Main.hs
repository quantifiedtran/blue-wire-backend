{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Brick as B
import qualified Brick.Widgets.List as B
import qualified Brick.Widgets.Core as B
import qualified Servant.Client as S
import BlueWire.Types
import BlueWire.Servant
import BlueWire.Database.Opaleye.Schema (Profile')
import qualified Data.Aeson as JSON
import qualified Graphics.Vty.Attributes as B (defAttr, reverseVideo, withStyle)

main :: IO ()
main = B.defaultMain bwapp (BWVState (MenuView $ menuList Menu) "~/.bwvconf") >> return ()

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

defaultVC = ViewConfig False Nothing

data BWVSum
    = ServerView
        { currentServer :: Maybe (String, Maybe Profile')
        , knownServers :: [(String, [String])]
        }
    | ConfigView { bwvConfig :: ViewConfig }
    | MenuView { brickList :: B.List ViewName ViewName }
    deriving (Show)

data BWVState = BWVState
    { view :: BWVSum
    , confFile :: String
    } deriving (Show)

renderState :: BWVState -> [B.Widget ViewName]
renderState BWVState{..} = [wig]
    where
        wig = case view of
                MenuView{..} ->
                    let itemRender slct = selected slct . \case
                            Menu -> B.str "menu"
                            Server -> B.str "select server"
                            LocalConfig -> B.str "config"
                    in B.renderList itemRender True brickList
                ServerView{..} -> undefined
                ConfigView{..} -> undefined

        selected :: Bool -> B.Widget n -> B.Widget n
        selected True w = B.withAttr "inverted" w
        selected _ w = w


menuList :: Ord n => n -> B.List n ViewName
menuList n =
    let list = [Server, LocalConfig]
    in B.list n list 1

handleEvents :: BWVState
             -> B.BrickEvent ViewName BWEvent
             -> B.EventM ViewName (B.Next BWVState)
handleEvents st@BWVState{..} ev =
    case view of
      menu@MenuView{..} -> case ev of
            B.VtyEvent vtyev -> do
                brickList_ <- B.handleListEvent vtyev brickList
                B.continue $ st{ view = menu{ brickList = brickList_ } }
            _ -> B.continue st

bwapp = B.App
    { appDraw = renderState
    , appChooseCursor = B.neverShowCursor
    , appStartEvent = return
    , appAttrMap
      = const $ B.attrMap B.defAttr [("inverted", B.defAttr `B.withStyle` B.reverseVideo)]
    , appHandleEvent = handleEvents
    }
