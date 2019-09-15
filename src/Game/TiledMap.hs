{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.TiledMap
  ( TiledMap(..)
  , tileset
  , tiles
  , TiledMapInfo(..)
  , load
  ) where

import Data.Maybe (fromJust)
import Control.Lens (makeLenses)
import Control.Exception (Exception, throw)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import Graphics.Gloss (Picture)
import Data.Aeson.Tiled hiding (Vector)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Aeson.Tiled (loadTiledmap, tiledmapLayers)
import qualified Data.Aeson.Tiled as Tiled

import Game.Image (ImageInfo(..))
import qualified Game.Image as Image

-- | Represents a tiled map.
data TiledMap = TiledMap
  { _tileset :: !Picture
  , _tiles :: !(Vector GlobalId)
  }

makeLenses ''TiledMap

data TiledMapInfo = TiledMapInfo
  { jsonPath :: !FilePath
  , tilesetImage :: !ImageInfo
  }

newtype TiledMapLoadException = TiledMapLoadException (TiledMapInfo, String)
instance Exception TiledMapLoadException

instance Show TiledMapLoadException where
  show (TiledMapLoadException (TiledMapInfo{..}, msg)) =
    unlines [ "Failed to load tiled map: " <> jsonPath, msg ]

load :: MonadIO m => TiledMapInfo -> m TiledMap
load info@TiledMapInfo{..} = do
  _tileset <- Image.loadInfo tilesetImage
  tiledMap <- liftIO $ loadTiledmap jsonPath
  let _tiles = mkTiles $ either err id tiledMap
  return TiledMap{..}
  where
    err msg = throw $ TiledMapLoadException (info, msg)

mkTiles :: Tiledmap -> Vector GlobalId
mkTiles tilemap =
  let layer = Vector.head $ tiledmapLayers tilemap
  in fromJust $ Tiled.layerData layer
