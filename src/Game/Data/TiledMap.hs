{-# LANGUAGE OverloadedStrings #-}

module Game.Data.TiledMap
  ( TiledMap(..)
  -- , load
  ) where

import Control.Exception (Exception)
import Graphics.Gloss (Picture(..))
import Data.Aeson.Tiled hiding (Vector)
import qualified Data.Vector as V
import Game.Image (Image)
import qualified Game.Image as Image

newtype MapLoadException = MapLoadException String deriving (Eq)
instance Exception MapLoadException

instance Show MapLoadException where
  show (MapLoadException msg) = "Failed to load map: " <> msg

-- | Represents a game map.
data TiledMap = TiledMap
  { texture :: Picture
  , tiles :: V.Vector GlobalId
  }

-- data TmxInfo = TmxInfo
--   { jsonPath :: FilePath
--   , tilesetPath :: FilePath
--   , tilesetImage
--   }

-- load :: FilePath -> FilePath -> IO TiledMap
-- load fileName = do
--   tiledMap <- Tiled.loadTiledmap fileName
--   return Map{..}
