{-# LANGUAGE RecordWildCards #-}

module Game.Data.TiledMap
  ( TiledMap(..)
  -- , load
  ) where

import Control.Exception (Exception)
import Graphics.Gloss (Picture(..))
import qualified Data.Aeson.Tiled as Tiled
import Data.Aeson.Tiled hiding (Vector)
import qualified Data.Vector as V
import Data.Bits (clearBit, testBit)

data MapLoadException = MapLoadException String deriving (Eq)
instance Exception MapLoadException

instance Show MapLoadException where
  show (MapLoadException msg) = "Failed to load map: " <> msg

-- | Represents a game map.
data TiledMap = TiledMap
  { texture :: Picture
  , tiles :: V.Vector GlobalId
  }

-- load :: FilePath -> FilePath -> IO TiledMap
-- load fileName = do
--   tiledMap <- Tiled.loadTiledmap fileName
--   return Map{..}
