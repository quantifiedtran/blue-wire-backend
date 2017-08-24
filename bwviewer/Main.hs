{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Brick as B
import qualified Servant.Client as S
import BlueWire.Types
import BlueWire.Servant
import BlueWire.Database.Opaleye (Profile')
import qualified Data.Aeson as JSON

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
    currentNav :: Nav
} deriving (Eq, Ord, Show)

renderState :: BWVState -> [Wiget ViewName]
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
bwapp = B.App
    { appDraw = renderState
    , appChooseCursor = \_ _ -> Nothing
    }
