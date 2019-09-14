{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Data.TiledMap
  ( TiledMap(..)
  , load
  ) where

import Data.Maybe (fromJust)
import Control.Exception (Exception, throw)
import Graphics.Gloss (Picture)
import Data.Aeson.Tiled hiding (Vector)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Game.Image (Image, ImageInfo(..))
import qualified Game.Image as Image
import qualified Data.Aeson.Tiled as Tiled
import Data.Aeson.Tiled (loadTiledmap, tiledmapLayers)

-- | Represents a tiled map.
data TiledMap = TiledMap
  { tileset :: Picture
  , tiles :: Vector GlobalId
  }

data TiledMapInfo = TiledMapInfo
  { jsonPath :: FilePath
  , tilesetImage :: ImageInfo
  }

newtype TiledMapLoadException = TiledMapLoadException (TiledMapInfo, String)
instance Exception TiledMapLoadException

instance Show TiledMapLoadException where
  show (TiledMapLoadException (TiledMapInfo{..}, msg)) =
    unlines [ "Failed to load tiled map: " <> jsonPath, msg ]

load :: TiledMapInfo -> IO TiledMap
load info@TiledMapInfo{..} = do
  tileset <- Image.loadInfo tilesetImage
  tiledmap <- loadTiledmap jsonPath
  let tiles = mkTiles $ either err id tiledmap
  return TiledMap{..}
  where
    err msg = throw $ TiledMapLoadException (info, msg)

mkTiles :: Tiledmap -> Vector GlobalId
mkTiles tilemap =
  let layer = Vector.head $ tiledmapLayers tilemap
  in fromJust $ Tiled.layerData layer
