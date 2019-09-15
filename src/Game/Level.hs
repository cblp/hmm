{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game.Level
  ( Level(..)
  , name
  , assets
  , tiledMap
  , load
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Lens (makeLenses)
import qualified Data.HashMap.Strict as HashMap

import Game.Image (Image(..), ImageInfo(..))
import Game.TiledMap (TiledMap(..), TiledMapInfo(..))
import qualified Game.TiledMap as TiledMap
import Game.Assets (Assets(..))

data Level = Level
  { _name :: Text
  , _assets :: Assets
  , _tiledMap :: TiledMap
  }

makeLenses ''Level

load :: Text -> IO Level
load _name = do
  _assets <- loadAssets _name
  _tiledMap <- loadTiledMap _name
  return Level{..}

loadAssets :: Text -> IO Assets
loadAssets _ = do
  let
    _sfxs = HashMap.empty
    _soundtracks = HashMap.empty
    _pictures = HashMap.empty
  return Assets{..}

loadTiledMap :: Text -> IO TiledMap
loadTiledMap levelName = do
  let imagePath = Text.unpack $ mapsPath <> "tileset.png"
      imageType = PNG
      tilesetImage = ImageInfo { imagePath, imageType }
      jsonPath = Text.unpack $ mapsPath <> "map.json"
  TiledMap.load $ TiledMapInfo { jsonPath, tilesetImage }
  where
    mapsPath = "assets/levels/" <> levelName <> "/maps/"
